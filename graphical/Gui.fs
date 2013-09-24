module Vindaloo.Gui

open Vindaloo.Architecture
open Vindaloo.Machine

open System.Windows
open System.Windows.Controls

let heappanel =
    let layout = Grid(ShowGridLines = true)

    layout.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    layout.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition()) |> ignore

    let t = Label(Content="Heap")
    let n = TextBox()
    let b = Button(Content="display")
    let oldhandler = ref (b.Click.Subscribe(ignore))

    let init callback size =
        (!oldhandler).Dispose()
        t.Content <- sprintf "Heap size: %i" size
        let handler _ =
            try
                let a = (int n.Text)
                if(a < size)
                then callback (int n.Text)
            with
              | :? System.FormatException -> ignore 0
        oldhandler := b.Click.Subscribe(handler)

    layout.Children.Add(t) |> ignore
    Grid.SetRow(t, 0)
    Grid.SetColumn(t, 0)
    Grid.SetColumnSpan(t, 2)
    layout.Children.Add(n) |> ignore
    Grid.SetRow(n, 1)
    Grid.SetColumn(n, 0)
    layout.Children.Add(b) |> ignore
    Grid.SetRow(b, 1)
    Grid.SetColumn(b, 1)
    
    layout, init
    
let updstring = "=== UPDATE ==="

type RetStackItem(e: Continuation option) =
    override this.ToString() =
        match e with
        | Some c -> sprintf "continuation"
        | None -> updstring
    member this.longText =
        match e with
        | Some c -> sprintf "%A" e
        | None -> "an update will be performed here"

let label text control =
    let ret = Grid()
    let lbl = Label(Content = text)
    lbl.FontWeight <- FontWeights.Bold

    ret.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    ret.RowDefinitions.Add(RowDefinition(Height=GridLength.Auto)) |> ignore
    ret.RowDefinitions.Add(RowDefinition()) |> ignore

    ret.Children.Add(lbl) |> ignore
    Grid.SetRow(lbl, 0)
    Grid.SetColumn(lbl, 0)
    ret.Children.Add(control) |> ignore
    Grid.SetRow(control, 1)
    Grid.SetColumn(control, 0)

    ret



let mainWindow (machine : STGMachine) = 
    let layout = Grid()
    let heapt = TextBox(IsReadOnly=true)
            
    let codeb = TextBox(IsReadOnly=true)
    let argl = ListBox()
    let updl = ListBox()
    let retl = ListBox()
    let globl = ListBox()
    let heapp, heapupd = heappanel
    let nextb = Button(Content="next step")

    let current = ref machine

    let subscriptions : System.IDisposable list ref= ref []

    let update machine = 
        match machine with
        | { code = code; argstack = argstack; retstack = retstack;
            updstack = updstack; heap = heap; globals = globals } ->
            let setheapt i = heapt.Text <- sprintf "%A" heap.[i]

            ! subscriptions |>
            List.map (fun s -> s.Dispose()) |> ignore

            codeb.Text <- sprintf "%A" code

            let stringify a = match a with
                | (Addr a) -> sprintf "Addr %i" a
                | (Int a) -> sprintf "Int %i" a

            let argstack' = List.map stringify argstack
            let updstack'arg = List.map ((fun a -> List.map stringify a.argstack):UpdateFrame -> string list) updstack
            let retstack' = List.map (fun a -> new RetStackItem (Some a)) retstack
            let updstack'ret = List.map ((fun a -> List.map (fun b -> new RetStackItem (Some b)) a.retstack):UpdateFrame -> RetStackItem list) updstack

            argl.ItemsSource <-
                List.fold (fun a b -> List.append a (updstring::b)) argstack' updstack'arg
            updl.ItemsSource <- updstack
            retl.ItemsSource <-
                List.fold (fun a b -> List.append a (new RetStackItem(None)::b)) retstack' updstack'ret
            globl.ItemsSource <- Map.toList globals
            heapupd setheapt heap.Length
        
            subscriptions := [
                globl.SelectionChanged.Subscribe
                    (fun s -> snd (globl.SelectedItem :?> string*int) |> setheapt)
                retl.SelectionChanged.Subscribe
                    (fun s ->
                        let a = (retl.SelectedItem :?> RetStackItem)
                        heapt.Text <- a.longText)
            ]

            ignore 0

    nextb.Click.Add(fun _ ->
        current := match step (! current) with
          | Running m -> m
          | Error (_, m) -> m
          | Finished m -> m
        update (!current))

    layout.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    layout.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    layout.ColumnDefinitions.Add(ColumnDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition(Height=GridLength(100.0))) |> ignore
    layout.RowDefinitions.Add(RowDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition(Height=GridLength.Auto)) |> ignore
    layout.RowDefinitions.Add(RowDefinition(Height=GridLength.Auto)) |> ignore

    let put control text row col rowspan colspan =
        let newc = label text control
        layout.Children.Add(newc) |> ignore
        Grid.SetRow(newc, row)
        Grid.SetColumn(newc, col)
        Grid.SetRowSpan(newc, rowspan)
        Grid.SetColumnSpan(newc, colspan)

    put codeb "Machine Code" 0 0 1 1
    put argl "Argument Stack" 0 1 2 1
    put retl "Return Stack" 0 2 2 1
    put globl "Globals" 2 1 1 1
    put heapp "Heap" 2 2 1 1 
    put heapt "Heap Cell" 1 0 3 1

    layout.Children.Add(nextb) |> ignore
    Grid.SetRow(nextb, 3)
    Grid.SetColumn(nextb, 1)
    Grid.SetColumnSpan(nextb, 2)
    
    (*
    layout.Children.Add(codeb) |> ignore
    Grid.SetRow(codeb, 0)
    Grid.SetColumn(codeb, 0)
    //Grid.SetColumnSpan(codeb, 2)
    layout.Children.Add(nextb) |> ignore
    Grid.SetRow(nextb, 3)
    Grid.SetColumn(nextb, 1)
    Grid.SetColumnSpan(nextb, 2)
    layout.Children.Add(argl) |> ignore
    Grid.SetRow(argl, 0)
    Grid.SetColumn(argl, 1)
    Grid.SetRowSpan(argl, 2)
    //layout.Children.Add(updl) |> ignore
    //Grid.SetRow(updl, 0)
    //Grid.SetColumn(updl, 1)
    layout.Children.Add(retl) |> ignore
    Grid.SetRow(retl, 0)
    Grid.SetColumn(retl, 2)
    Grid.SetRowSpan(retl, 2)
    layout.Children.Add(globl) |> ignore
    Grid.SetRow(globl, 2)
    Grid.SetColumn(globl, 1)
    layout.Children.Add(heapp) |> ignore
    Grid.SetRow(heapp, 2)
    Grid.SetColumn(heapp, 2)
    layout.Children.Add(label "Heap Cell" heapt) |> ignore
    Grid.SetRow(heapt, 1)
    Grid.SetColumn(heapt, 0)
    Grid.SetRowSpan(heapt, 3)
    *)

    update machine

    Window(Content=layout)