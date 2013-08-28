module Vindaloo.Ui

open System.Windows

[<System.STAThreadAttribute>]
do
    Application().Run(Window(Content="Hello World")) |> ignore