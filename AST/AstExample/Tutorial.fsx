module AST

type distance = int
type degrees = int
type count = int
type command =
   | Forward of distance
   | Left of degrees
   | Right of degrees
   | Repeat of count * command list