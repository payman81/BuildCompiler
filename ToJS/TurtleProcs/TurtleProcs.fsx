open System.IO

// ============================
// Task
// Get generated AST for Garden
// Transform AST to valid JS
// ============================

let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle.html")
path |> System.Diagnostics.Process.Start 