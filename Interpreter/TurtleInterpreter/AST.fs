[<AutoOpen>]
module AST

type colour = Red | Green | Blue
type arg = int
type command =
   | Forward of arg
   | Back of arg
   | Turn of arg
   | Pen of colour
   | Repeat of arg * command list
