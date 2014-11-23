﻿open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let notImplemented () = raise (System.NotImplementedException())

let toTypeName  t =
    if t = typeof<int> then "int"
    elif t = typeof<double> then "double" 
    elif t = typeof<bool> then "boolean"
    else notImplemented()

let name =
    let x = ref 0
    fun () -> incr x; sprintf "_%d" !x

let rec toJava (add:string -> unit) = function
   | Let(var, expr, body) -> toLet add var expr body
   | Var(var) -> sprintf "%s" var.Name
   | VarSet(var, expr) -> toAssign add var expr
   | Int32 x -> sprintf "%i" x
   | Double x -> sprintf "%gd" x
   | Bool true -> "true"
   | Bool false -> "false"
   | SpecificCall <@@ (+) @@> (None, _, [lhs;rhs]) -> toArithOp add "+" lhs rhs
   | SpecificCall <@@ (-) @@> (None, _, [lhs;rhs]) -> toArithOp add "-" lhs rhs
   | SpecificCall <@@ (*) @@> (None, _, [lhs;rhs]) -> toArithOp add "*" lhs rhs
   | SpecificCall <@@ (/) @@> (None, _, [lhs;rhs]) -> toArithOp add "/" lhs rhs
   | SpecificCall <@@ (=) @@> (None, _, [lhs;rhs]) -> toLogicOp add "==" lhs rhs
   | SpecificCall <@@ (<>) @@> (None, _, [lhs;rhs]) -> toLogicOp add "!=" lhs rhs
   | IfThenElse(condition, t, f) -> toIfThenElse add condition t f
   | Sequential(lhs,rhs) -> toSequential add lhs rhs
   | ForIntegerRangeLoop(var,Int32 a,Int32 b,body) -> toForLoop add var a b body
   | _ -> notImplemented()
and toLet add var expr body =
    let valueName = toJava add expr
    add <| sprintf "%s %s = %s;" (toTypeName var.Type) var.Name valueName
    toJava add body
and toAssign add var expr =
    let value = toJava add expr
    add <| sprintf "%s = %s;" var.Name value
    ""
and toArithOp add op lhs rhs =
    let l,r = (toJava add lhs), (toJava add rhs)
    let name = name ()
    add <| sprintf "%s %s = (%s %s %s);" (toTypeName lhs.Type) name l op r
    name
and toLogicOp add op lhs rhs =
    let l,r = (toJava add lhs), (toJava add rhs)
    let name = name ()
    add <| sprintf "boolean %s = (%s %s %s);" name l op r
    name
and toIfThenElse add condition t f =
    let cn, tn, fn = toJava add condition, toJava add t, toJava add f
    let name = name ()
    add <| sprintf "%s %s = %s ? %s : %s;" (toTypeName t.Type) name cn tn fn
    name
and toSequential add lhs rhs =    
    toJava add lhs |> ignore
    toJava add rhs
and toForLoop add var a b body =
    let s = System.Text.StringBuilder()
    let localAdd (x:string) = s.Append(x) |> ignore
    toJava localAdd body |> ignore
    let i = var.Name
    add <| sprintf "for(int %s = %d; %s <= %d; %s++) { %s }" i a i b i (s.ToString())
    ""

let toClass (expr:Expr<'TRet>) =
    let t = typeof<'TRet>
    if t = typeof<unit> then notImplemented()
    let returnType = toTypeName t
    let s = System.Text.StringBuilder()
    let add x = s.AppendLine("    " + x) |> ignore
    let v = toJava add expr
    sprintf """
public class Generated {
  public static %s fun(){
%s    return %s;
  }
  public static void main(String []args){
    System.out.println(fun());
  }
}"""  returnType (s.ToString()) v

toClass 
 <@ let mutable fac = 1
    for i = 1 to 6 do fac <- fac * i
    fac @>

(*Returns

public class Generated {
  public static int fun(){
    int fac = 1;
    for(int i = 1; i <= 6; i++) { int _1 = (fac * i);fac = _1; }
    return fac;
  }
  public static void main(String []args){
    System.out.println(fun());
  }
}

*)