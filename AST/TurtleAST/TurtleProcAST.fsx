module AST 

type name = string
type param = string
type arg = Number of int | Arg of param
type command =
   | Forward of arg
   | Left of arg
   | Right of arg
   | SetRandomPosition
   | Repeat of arg * command list
   | Call of name * arg list
   | Proc of name * param list * command list
