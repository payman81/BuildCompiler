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

(* Logo Fractal tree
// http://rosettacode.org/wiki/Fractal_tree#Logo
to tree :depth :length :scale :angle
  if :depth=0 [stop]
  forward :length
  right :angle
  tree :depth-1 :length*:scale :scale :angle
  left 2*:angle
  tree :depth-1 :length*:scale :scale :angle
  right :angle
  back :length
end

tree 10 80 0.7 30
*)
let program = 
   [
   Proc("tree", ["depth"; "length"; "scale"; "angle"],
        [
        If(Condition(Arg("depth"),Eq,Number(0.0)),[Stop])
        Forward(Arg("length"))
        Right(Arg("angle"))
        Call("tree",[Arithmetic(Arg("depth"),Subtract,Number(1.0));
                     Arithmetic(Arg("length"),Multiply,Arg("scale"));
                     Arg("scale");
                     Arg("angle")])
        Left(Arithmetic(Number(2.0),Multiply,Arg("angle")))
        Call("tree",[Arithmetic(Arg("depth"),Subtract,Number(1.0));
                     Arithmetic(Arg("length"),Multiply,Arg("scale"));
                     Arg("scale");
                     Arg("angle")])
        Right(Arg("angle"))
        Back(Arg("length"))
        ])
   Call("tree", [Number(10.0); Number(80.0); Number(0.7); Number(30.0)])
   ]

open System

let getVar = 
   let varId = ref 0
   fun () -> incr varId; sprintf "_%d" !varId 

let rec emitBlock indent commands =
   String.concat "" [|for command in commands -> emitCommand indent command|]  
and emitCommand indent command =
   let tabs = String.replicate indent "\t"
   match command with
   | Forward(e) -> sprintf "forward(%s);" (emitExpr e)
   | Back(e) -> sprintf "back(%s);" (emitExpr e)
   | Left(e) -> sprintf "left(%s);" (emitExpr e)
   | Right(e) -> sprintf "right(%s);" (emitExpr e)
   | Repeat(e,commands) ->
      let block = emitBlock (indent+1) commands
      String.Format("for({0}=0;{0}<{1};{0}++) {{\r\n {2}}}", getVar(), emitExpr e, block); 
   | Make(name,e) -> sprintf "var %s = %s;" name (emitExpr e)
   | If(Condition(lhs,op,rhs),commands) ->
      let condition = sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
      sprintf "if(%s) {\r\n%s%s}" condition (emitBlock (indent+1) commands) tabs
   | Stop -> "return;"
   | Proc(name,``params``,commands) ->
      sprintf "\r\nfunction %s(%s) {\r\n%s%s}" 
         name 
         (String.concat "," ``params``) 
         (emitBlock (indent+1) commands) 
         tabs
   | Call(name,args) ->
      sprintf "%s(%s);" name (String.concat "," [for arg in args -> emitExpr arg])
   | _ -> failwith "Not implemented"
   |> fun s -> tabs + s + "\r\n"
and emitExpr expr =
   match expr with
   | Number(n) -> String.Format("{0}", n)
   | String(s) -> sprintf "\"%s\"" s
   | Arg(s) -> s
   | Var(s) -> s
   | Arithmetic(lhs,op,rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromArith op) (emitExpr rhs)
   | Comparison(lhs,op,rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
   | Logical(lhs,op,rhs) -> failwith "Not implemented"
and fromArith op =
   match op with
   | Add -> "+"
   | Subtract -> "-"
   | Divide -> "/"
   | Multiply -> "*"
and fromComparison op = 
   match op with
   | Eq -> "=="
   | Ne -> "!="
   | Lt -> "<"
   | Gt -> ">"
   | Le -> "<="
   | Ge -> ">=" 

emitBlock 0 program


open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle.html")
path |> System.Diagnostics.Process.Start 