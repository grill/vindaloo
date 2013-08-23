module Vindaloo.Util

let rec ListSplit list endIdx = 
    match list with
    | xs when endIdx = 0 -> Some ([], xs)
    | h :: tail ->
        match ListSplit tail (endIdx-1) with
        | Some (alist,elist) -> Some (h :: alist, elist)
        | x -> x
    | _ -> None

let OptionAnd tf1 tf2 f =
    match tf1 with
    | None -> None
    | Some x ->
        match tf2 with
        | None -> None
        | Some y -> Some (f x y)

let OptionOr tf1 (tf2 : Lazy<Option<'T>>) =
    match tf1 with
    | None -> tf2.Force()
    | x -> x

let rec OptionListMap p g xs f =
    match xs with
    | h::tail -> OptionAnd (f p g h) (OptionListMap p g tail f) (fun v vs -> v :: vs)
    | [] -> Some []
