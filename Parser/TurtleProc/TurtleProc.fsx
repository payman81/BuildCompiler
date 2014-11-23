﻿module AST =
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
         | Proc(name, ps, commands) -> procs := Map.add name (ps,commands) !procs; turtle
         | Call(name,args) -> 
            let ps, commands = (!procs).[name]
            if ps.Length <> args.Length then raise (ArgumentException("Parameter count mismatch"))
            let xs = List.zip ps args
            let env = xs |> List.fold (fun e (name,value) -> Map.add name value env) env            
            commands |> performAll env turtle
      and performAll env turtle commands = commands |> List.fold (perform env) turtle
      and getValue env = function 
        | Number n -> n
        | Arg name -> getValue env (Map.tryFind name env).Value
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
   let pnumber = pfloat |>> fun n -> Number(int n)

   let parg = pnumber <|> (pparam |>> fun a -> Arg(a))

   let pforward = 
      (pstring "forward" <|> pstring "fd") >>. spaces1 >>. parg 
      |>> fun arg -> Forward(arg)
   let pleft = 
      (pstring "left" <|> pstring "lt") >>. spaces1 >>. parg 
      |>> fun arg -> Left(arg)
   let pright = 
      (pstring "right" <|> pstring "rt") >>. spaces1 >>. parg
      |>> fun arg -> Right(arg)
   let prandom = 
      pstring "set-random-position"
      |>> fun _ -> SetRandomPosition
   let prepeat, prepeatimpl = createParserForwardedToRef ()
   let pcall, pcallimpl = createParserForwardedToRef ()

   let pcommand = pforward <|> pleft <|> pright <|> prandom <|> prepeat <|> pcall

   let updateCalls () =
      pcallimpl :=          
         choice [
            for (name,ps) in !procs -> 
                pstring name >>. spaces >>. (many parg .>> spaces)
                |>> fun args -> Call(name,args)
         ]
   updateCalls()

   let block = between (pstring "[" .>> spaces) (pstring "]") 
                       (sepEndBy pcommand spaces1)
   
   prepeatimpl := 
      pstring "repeat" >>. spaces1 >>. parg .>> spaces .>>. block
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
     repeat 4 [forward 50 right 90]
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