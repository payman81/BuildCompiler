open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle.html")
path |> System.Diagnostics.Process.Start 