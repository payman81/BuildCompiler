type packageName = string
type typeName = string
type name = string
type typ = string
type value =
   | Bool of bool
   | Int of int
   | Float64 of double
   | String of string  
// Note: standard arithmetic, conditional & logical operations omitted for brevity
type expr =
   | Value of value
   | ReadVar of name
   | ReadField of typeName * name  
   | Cast of System.Type * expr
   | Func of invoke
   | NewStruct of name * expr list   
and invoke =
   | InvokeFunction of packageName option * name * expr list
   | InvokeMethod of typeName * name * expr list
type statement =
   | DeclareVar of name * typ
   | AssignVar of name * expr
   | SetField of name * expr
   | Action of invoke
   | Return of expr
   | For of statement option * expr * statement option * block
   | While of expr option * block
   | If of statement option * expr option * block
   | Switch of statement option * expr option * case list
and case = Case of expr * block
and block = statement list

type arg = Arg of name * System.Type
type field = Field of name * System.Type
type definition =
   | Package of packageName
   | Import of packageName list
   | DeclareVar of name * typ
   | Function of name * arg list * typ option * block
   | Struct of name * field list

type package = definition list

