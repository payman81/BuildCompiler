// Turtle language with Expressions

type name = string
type param = string
type arithmetic =
   Add | Subtract | Multiply | Divide
type comparison =
   Eq | Ne | Lt | Gt | Le | Ge
type logical =
   And | Or
type expr =
   | Number of float 
   | String of string
   | Arg of param 
   | Var of name
   | Arithmetic of expr * arithmetic * expr
   | Comparison of expr * comparison * expr
   | Logical of expr * logical * expr
type condition =
   | Condition of expr * comparison * expr
type command =
   // Begin built-in functions
   | Forward of expr
   | Back of expr
   | Left of expr
   | Right of expr
   | Random of expr
   | SetRandomPosition
   // End built-in functions
   | Repeat of expr * command list
   | Call of name * expr list
   | Proc of name * param list * command list
   | Make of name * expr
   | If of condition * command list
   | Stop
