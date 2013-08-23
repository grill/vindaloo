module Vindaloo.Machine

open FParsec
open Vindaloo.Parser
open Vindaloo.Architecture
open Vindaloo.Util

let valueVar p g v = OptionOr (Map.tryFind v p) (lazy (Map.tryFind v g |> Option.map Addr))

let value p g x =
    match x with
    | Syntax.LiteralA k -> Some (Int k)
    | Syntax.VarA v -> valueVar p g v

let EnvironmentAdd hlen p binds =
    let p', _ = Map.fold (fun (g, i) name _ -> (Map.add name (Addr i) g, i+1)) (p, hlen) binds
    p'

let HeapAdd h prhs binds =
    let hn = Map.fold (fun l _ (code : Syntax.LambdaForm) ->
        (fun l ws -> (code, ws) :: l) |>
        OptionAnd l (OptionListMap prhs Map.empty code.freeVars valueVar)) (Some []) binds
    match hn with
    | Some (hn) -> Some (List.toArray hn |> Array.append h)
    | _ -> None

//Perform one computational step of the STG-machine.
//references above the rules are to the STG-machine paper
let step machine : STGState =
    match machine with
    
    //5.2 Applications (1) - tail call
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p); globals = g; argstack = a } ->
        match (value p g (Syntax.VarA f), OptionListMap p g xs value) with
        | (Some (Addr addr), Some (vList)) ->
            Running {
            machine with
                code = Enter (addr);
                argstack = List.append vList a
            }
        | _ -> Error ("Tail call failed", machine)
        
    //5.2 Applications (2) - enter function
    | { code = Enter addr; heap = h; globals = g; argstack = a } when Array.length h > addr ->
        match h.[addr] with
        | ({ freeVars = vs; updateable = false; parameters = xs; body = e}, wsf)
            when List.length a >= List.length xs ->
            match List.length xs |> ListSplit a with
            | Some (a', wsa) ->
                let p = List.zip vs wsf |> List.append (List.zip xs wsa) |> Map.ofList
                Running {
                  machine with
                    code = Eval (e, p); 
                    argstack = a'
                }
            | _ -> Error("Enter failed!", machine)
        | _ -> Error("Enter failed!", machine)
        
    //5.3 let(rec) Expressions (3) - let | letrec
    | { code = Eval (Syntax.LetE {binds = binds; expr = e}, p) ; heap = h }
    | { code = Eval (Syntax.LetrecE {binds = binds; expr = e}, p) ; heap = h } ->
        let p' = EnvironmentAdd h.Length p binds
        let prhs =
            match machine.code with
            | Eval (Syntax.LetE {expr = _}, _) -> p
            | _ -> p'
        match HeapAdd h prhs binds with
        | Some (h') ->
            Running {
                machine with
                    code = Eval (e, p'); 
                    heap = h'
            }
        | _ -> Error("Let(rec) failed!", machine)
        
    //5.4 Case expressions and data constructors (4) - case
    | { code = Eval (Syntax.CaseE {alts = alts; expr = e}, p) ; retstack = rs } ->
        Running {
            machine with
                code = Eval (e, p);
                retstack = (alts, p) :: rs
        }

    //5.4 Case expressions and data constructors (5) - evaluate constructor
    | { code = Eval (Syntax.ConstrApplE {constr = c; pars = xs}, p) ; globals = g } ->
        match OptionListMap p g xs value with
        | Some ws ->
            Running {
                machine with code = ReturnCon (c, ws)
            }
        | _ -> Error ("Evaluate Constructor failed!", machine)
    
    //5.4 Case expressions and data constructors (6-8) - choose continuation
    | { code = ReturnCon (c, ws) ; retstack = (Syntax.AlgebraicAlts alts, p)::_ ; heap = h } ->
        // (6) - normal case expression
        match
            (if (Map.containsKey c alts.cases) then
                let vs, e = Map.find c alts.cases
                Some (e, List.zip vs ws |> List.append (Map.toList p) |> Map.ofList, h)
            else
                let v, e = alts.def
                match v with
                        
                // (7) - default case expression
                | [] -> Some (e, p, h)

                // (8) - default case expression with variable
                | v::[] ->
                    let a = h.Length
                    let cl = "v" + c
                    let vs = [1..ws.Length] |> List.map (fun x -> cl + string x ) 
                    let h' =
                        [| ( 
                            { Syntax.freeVars = vs; Syntax.updateable = false; Syntax.parameters = [];
                            Syntax.body = Syntax.ConstrApplE { constr = c; pars = List.map Syntax.VarA vs } }
                        , ws) |] |> Array.append h  
                    Some (e, Map.add v (Addr a) p, h')
                | _ -> None)
            with
        | Some (e, p', h') ->
            Running {
                machine with
                    code = Eval (e, p') ;
                    heap = h'
            }
        | None -> Error ("Choose Continuation failed!", machine)
        
    //5.5 Built in operations (9) - eval literal
    | { code = Eval (Syntax.LiteralE k, p) } ->
        Running {
            machine with
                code = ReturnInt (k)
        }

    //!!!!!!!!!!!!!!!!!!!!!!!! This rule should probably be moved above the (1) rule !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    //5.5 Built in operations (10) - eval literal parameter
    | { code = Eval (Syntax.ApplE {var = v; pars = [] }, p) }
        when (match Map.tryFind v p with | Some (Int _) -> true | _ -> false) ->
        match Map.find v p with
        | Int x ->
            Running {
                machine with
                    code = ReturnInt x
            }
        | _ -> Error ("Eval primitive parameter failed!", machine)
        
    //5.5 Built in operations (11) - case expression with literal
    | { code = ReturnInt k ; retstack = (Syntax.PrimitiveAlts alts, p)::_ } when Map.containsKey k alts.cases ->
        Running {
            machine with
                code = Eval (Map.find k alts.cases |> (fun (_,e) -> e), p)
        }
            
    //5.5 Built in operations (12-13) - default case expression with literal
    | { code = ReturnInt k ; retstack = (Syntax.PrimitiveAlts alts, p)::_ } ->
        let p', e = 
            match alts.def with
            // (12) - default case expression with vsariable
            | (v::[], e) -> (Map.add v (Int k) p, e)
            // (13) - default case expression
            | (_, e) -> (p, e)
        Running {
            machine with
                code = Eval (e, p')
         }
    
    //5.5 Built in operations (14) - eval arithmetic operation
    | { code = Eval (Syntax.PrimApplE { op = op ; pars = x1::x2::[] }, p) ; globals = g } ->
        match (value p g x1, value p g x2) with
        | (Some (Int i1), Some (Int i2)) ->
            Running {
                machine with
                    code = ReturnInt (op i1 i2)
            }
        | _ -> Error ("Eval arithmetic operation failed!", machine)

    //5.6 Updating (15) - enter updateable closure
    | { code = Enter a; heap = h ; updstack = u ; retstack = rs ; argstack = argst} 
        when Array.length h > a && (match h.[a] with | ({updateable = true}, _) -> true | _ -> false)->
        match h.[a] with
        | ({ freeVars = vs ; body = e }, ws) ->
            Running {
                machine with
                    code = Eval (e, List.zip vs ws |> Map.ofList) ;
                    updstack = {argstack=argst ; retstack=rs; closure=a}::u
            }
        | _ -> Error ("Enter updateable closure failed!", machine)

    //5.6 Updating (17) - not enough arguments to trigger update
    | { code = Enter a; heap = h ; updstack = {argstack=argstu ; retstack=rsu; closure=au}::u ; retstack = [] ; argstack = argst} 
        when Array.length h > a &&
            (match h.[a] with
            | ({updateable = false ; parameters = xs}, _) ->
                List.length xs > 0 && List.length argst < List.length xs
            | _ -> false) ->
        match h.[a] with
        | ({ freeVars = vs ; parameters = xs ; body = e }, ws) ->
            let xs1, xs2 = ListSplit xs (List.length argst) |> Option.get
            Array.set h au (
                {freeVars = List.append vs xs1; updateable = false;
                 parameters = xs2; body = e;}
            , List.append ws argst) 
            Running {
                machine with
                    code = Enter a ;
                    argstack = List.append argst argstu ;
                    retstack = rsu ;
            }
        | _ -> Error ("Not enough arguments to trigger update failed!", machine)

    | _ -> Error ("STG-Machine failed!", machine) //or the machine is finished

let initSTG code =
    let g, _ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code
    let h = Map.foldBack (fun name addr h -> ( (Map.find name code) :: h))
                        g ([])
    {
        argstack = []
        retstack = []
        updstack = []
        heap = h |> List.toArray
        globals = g
        code = Eval (Syntax.ApplE {var = "main"; pars = []}, Map [])
    }
