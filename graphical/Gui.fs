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


let mainWindow (machine : STGMachine) = 
    let layout = Grid(ShowGridLines=true)
    let heapt = TextBox(IsReadOnly=true)
        
    let codeb = TextBox(IsReadOnly=true)
    let argl = ListBox()
    let updl = ListBox()
    let retl = ListBox()
    let globl = ListBox()
    let heapp, heapupd = heappanel
    let nextb = Button(Content=">")

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
            argl.ItemsSource <- argstack
            updl.ItemsSource <- updstack
            retl.ItemsSource <- retstack
            globl.ItemsSource <- Map.toList globals
            heapupd setheapt heap.Length
        
            subscriptions := [
                globl.SelectionChanged.Subscribe
                    (fun s -> snd (globl.SelectedItem :?> string*int) |> setheapt)
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
    layout.RowDefinitions.Add(RowDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition()) |> ignore
    layout.RowDefinitions.Add(RowDefinition()) |> ignore

    layout.Children.Add(codeb) |> ignore
    Grid.SetRow(codeb, 0)
    Grid.SetColumn(codeb, 0)
    Grid.SetColumnSpan(codeb, 2)
    layout.Children.Add(nextb) |> ignore
    Grid.SetRow(nextb, 0)
    Grid.SetColumn(nextb, 2)
    layout.Children.Add(argl) |> ignore
    Grid.SetRow(argl, 1)
    Grid.SetColumn(argl, 0)
    layout.Children.Add(updl) |> ignore
    Grid.SetRow(updl, 1)
    Grid.SetColumn(updl, 1)
    layout.Children.Add(retl) |> ignore
    Grid.SetRow(retl, 1)
    Grid.SetColumn(retl, 2)
    layout.Children.Add(globl) |> ignore
    Grid.SetRow(globl, 2)
    Grid.SetColumn(globl, 0)
    layout.Children.Add(heapp) |> ignore
    Grid.SetRow(heapp, 2)
    Grid.SetColumn(heapp, 1)
    layout.Children.Add(heapt) |> ignore
    Grid.SetRow(heapt, 2)
    Grid.SetColumn(heapt, 2)
        
    update machine

    Window(Content=layout)