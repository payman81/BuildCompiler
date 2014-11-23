// Turtle language with Expressions

module AST =

   type name = string
   type param = string
   type arithmetic =
      Add | Subtract | Multiply | Divide
   type comparison =
      Eq | Ne | Lt | Gt | Le | Ge
   type logical =
      And | Or
   type expr =
      | Number of int 
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
      | IfElse of condition * command list * command list
      | Stop

(*[omit:Windows Forms references]*)
#if INTERACTIVE
#r "System.Drawing.dll"
#r "System.Windows.Forms.dll"
#endif
(*[/omit]*)

module Interpreter =
   open AST
   open System
   open System.Drawing
   open System.Windows.Forms

   type Turtle = { X:float; Y:float; A:int }

   let execute commands =
      let procs = ref Map.empty
      let width, height = 640, 480
      let form = new Form (Text="Turtle", Width=width, Height=height)
      let image = new Bitmap(width, height)
      let picture = new PictureBox(Dock=DockStyle.Fill, Image=image)
      do  form.Controls.Add(picture)
      let turtle = { X=float width/2.0; Y=float height/2.0; A = -90 }
      use pen = new Pen(Color.Red)
      let rand = let r = Random() in fun n -> r.Next(n) |> float
      let drawLine (x1,y1) (x2,y2) =
         use graphics = Graphics.FromImage(image)
         graphics.DrawLine(pen,int x1,int y1,int x2, int y2)
      let rec perform env turtle = function
         | Forward arg ->
            let r = float turtle.A * Math.PI / 180.0
            let n = getValue env arg
            let dx, dy = float n * cos r, float n * sin r
            let x, y =  turtle.X, turtle.Y
            let x',y' = x + dx, y + dy
            drawLine (x,y) (x',y')
            { turtle with X = x'; Y = y' }
         | Left arg -> { turtle with A=turtle.A - getValue env arg }
         | Right arg -> { turtle with A=turtle.A + getValue env arg }
         | SetRandomPosition -> { turtle with X=rand width; Y=rand height }
         | Repeat(arg,commands) ->
            let n = getValue env arg
            let rec repeat turtle = function
               | 0 -> turtle
               | n -> repeat (performAll env turtle commands) (n-1)
            repeat turtle n
         | If(Condition(lhs,Eq,rhs),commands) ->
            if getValue env lhs = getValue env rhs
            then commands |> performAll env turtle
            else turtle
         | IfElse(Condition(lhs,Eq,rhs),t,f) ->
            if getValue env lhs = getValue env rhs
            then t 
            else f
            |> performAll env turtle
         | Proc(name, ps, commands) -> procs := Map.add name (ps,commands) !procs; turtle
         | Call(name,args) -> 
            let ps, commands = (!procs).[name]
            if ps.Length <> args.Length then raise (ArgumentException("Parameter count mismatch"))
            let xs = List.zip ps args           
            let env = xs |> List.fold (fun e (name,value) -> Map.add name value env) env            
            commands |> performAll env turtle
         | _ -> failwith "Not implemented"
      and performAll env turtle commands = commands |> List.fold (perform env) turtle
      and getValue env = function
        | Number n -> n
        | Arg name -> getValue env (Map.tryFind name env).Value
        | Arithmetic(lhs,op,rhs) -> arith env lhs rhs op
        | _ -> failwith "Not implemented"
      and arith env lhs rhs = function
        | Add -> getValue env lhs + getValue env rhs
        | Subtract -> getValue env lhs - getValue env rhs
        | Multiply -> getValue env lhs * getValue env rhs
        | Divide -> getValue env lhs / getValue env rhs             
      performAll Map.empty turtle commands |> ignore
      form.ShowDialog() |> ignore

(*[omit:FParsec references]*)
#if INTERACTIVE
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#endif
(*[/omit]*)

module Parser =

   open AST
   open FParsec

   let procs = ref []

   let pidentifier =
      let isIdentifierFirstChar c = isLetter c || c = '-'
      let isIdentifierChar c = isLetter c || isDigit c || c = '-'
      many1Satisfy2L isIdentifierFirstChar isIdentifierChar  "identifier"

   let pparam = pstring ":" >>. pidentifier 
   let parg = pparam |>> fun a -> Arg(a)
   let pnumber = pfloat |>> fun n -> Number(int n)
   let pvalue = attempt pnumber <|> attempt parg

   type Assoc = Associativity
   let ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c='\r') // spaces
   let opp = new OperatorPrecedenceParser<expr,unit,unit>()
   let pexpr = opp.ExpressionParser
   let term = pvalue <|> between (pstring "(") (pstring ")") pexpr
   opp.TermParser <- term
   opp.AddOperator(InfixOperator("+", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
   opp.AddOperator(InfixOperator("-", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
   opp.AddOperator(InfixOperator("*", ws, 3, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
   opp.AddOperator(InfixOperator("/", ws, 3, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
   let comparisons = ["=",Eq; "<>",Ne; "<=",Le; ">=",Ge; "<",Lt; ">",Gt]
   for s,op in comparisons do
       opp.AddOperator(InfixOperator(s, ws, 2, Assoc.Left, fun x y -> Comparison(x, op, y)))

   let pforward = 
      (pstring "forward" <|> pstring "fd") >>. spaces1 >>. pexpr
      |>> fun arg -> Forward(arg)
   let pleft = 
      (pstring "left" <|> pstring "lt") >>. spaces1 >>. pexpr
      |>> fun arg -> Left(arg)
   let pright = 
      (pstring "right" <|> pstring "rt") >>. spaces1 >>. pexpr
      |>> fun arg -> Right(arg)
   let prandom = 
      pstring "set-random-position"
      |>> fun _ -> SetRandomPosition
   let prepeat, prepeatimpl = createParserForwardedToRef ()
   let pif, pifimpl = createParserForwardedToRef ()
   let pifelse, pifelseimpl = createParserForwardedToRef ()
   let pcall, pcallimpl = createParserForwardedToRef ()

   let pcommand = 
       pforward <|> pleft <|> pright <|> prandom <|> prepeat <|> pifelse <|> pif <|> pcall 

   let updateCalls () =
      pcallimpl :=
         choice [
            for (name,ps) in !procs -> 
                pstring name >>. spaces >>. (many pexpr .>> spaces)
                |>> fun args -> Call(name,args)
         ]
   updateCalls()

   let block = between (pstring "[" .>> spaces) (pstring "]") 
                       (sepEndBy pcommand spaces1)
   
   let pcondition = pexpr //.>> spaces .>> pstring "=" .>> spaces .>>. pexpr

   let toCondition = function
      | Comparison(lhs,op,rhs) -> Condition(lhs,op,rhs)
      | _ -> failwith "Invalid expression"

   pifimpl :=
      pstring "if" >>. spaces1 >>. pcondition .>> spaces .>>. block
      |>> fun (expr,commands) -> If(toCondition expr, commands)
   
   let twoblocks = block .>> spaces .>>. block
   pifelseimpl :=
      pstring "ifelse" >>. spaces1 >>. pcondition .>> spaces .>>. twoblocks
      |>> fun (expr,(t,f)) -> IfElse(toCondition expr, t, f)

   prepeatimpl := 
      pstring "repeat" >>. spaces1 >>. pexpr .>> spaces .>>. block
      |>> fun (arg,commands) -> Repeat(arg, commands)

   let pparams = many (pparam .>> spaces)
   let pheader = pstring "to" >>. spaces1 >>. pidentifier .>> spaces1 .>>. pparams
   let pbody = many (pcommand .>> spaces1)
   let pfooter = pstring "end"

   let pproc =
      pheader .>>. pbody .>> pfooter
      |>> fun ((name,ps),body) -> 
        procs := (name,ps)::!procs; updateCalls()
        Proc(name, ps, body)

   let parser =
      spaces >>. (sepEndBy (pcommand <|> pproc) spaces1)

   let parse code =
      match run parser code with
      | Success(result,_,_) -> result
      | Failure(msg,_,_) -> failwith msg

let code = "
   to square
     repeat 4 [ifelse 1=0 [forward 10 right 90] [forward 50 right 90]]
   end
   to flower
     repeat 36 [right 10 square]
   end
   to garden :count
     repeat :count [set-random-position flower]
   end
   garden 25
   "
let program = Parser.parse code
Interpreter.execute program