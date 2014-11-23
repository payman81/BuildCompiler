module AST =
   type distance = int
   type degrees = int
   type count = int
   type command =
      | Forward of distance     
      | Left of degrees
      | Right of degrees
      | Repeat of count * command list

(*[omit:Windows Forms references]*)
#if INTERACTIVE
#r "System.Drawing.dll"
#r "System.Windows.Forms.dll"
#endif
(*[/omit]*)

(*[omit:FParsec references]*)
#if INTERACTIVE
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"
#endif
(*[/omit]*)

module Parser =

   open AST
   open FParsec

   let pforward = 
      (pstring "forward" <|> pstring "fd") >>. spaces1 >>. pfloat 
      |>> fun x -> Forward(int x)
   let pleft = 
      (pstring "left" <|> pstring "lt") >>. spaces1 >>. pfloat 
      |>> fun x -> Left(int x)
   let pright = 
      (pstring "right" <|> pstring "rt") >>. spaces1 >>. pfloat 
      |>> fun x -> Right(int x)

   let prepeat, prepeatimpl = createParserForwardedToRef ()

   let pcommand = pforward <|> pleft <|> pright <|> prepeat

   let block = between (pstring "[") (pstring "]") (many1 (pcommand .>> spaces))

   prepeatimpl := 
      pstring "repeat" >>. spaces1 >>. pfloat .>> spaces .>>. block
      |>> fun (n,commands) -> Repeat(int n, commands)

   let parse code =
      match run (many pcommand) code with
      | Success(result,_,_) -> result
      | Failure(msg,_,_) -> failwith msg

let code = "repeat 10 [right 36 repeat 5 [forward 54 right 72]]"

open AST
open System
open System.Reflection
open System.Reflection.Emit

#r @"..\TurtleLibrary\bin\Debug\TurtleLibrary.dll"

let emitInstructions (il:ILGenerator) program =
   let rec emitCommand command =
      match command with
      | Forward(n) -> emitInvoke "Forward" [|n|]
      | Left(n) -> emitInvoke "Turn" [|-n|]
      | Right(n) -> emitInvoke "Turn" [|n|]
      | Repeat(0,commands) -> ()
      | Repeat(n,commands) ->         
         let local = il.DeclareLocal(typeof<int>)
         il.Emit(OpCodes.Ldc_I4, n)
         il.Emit(OpCodes.Stloc, local.LocalIndex)
         let label = il.DefineLabel()                       
         il.MarkLabel(label)
         for command in commands do emitCommand command
         il.Emit(OpCodes.Ldloc, local.LocalIndex)
         il.Emit(OpCodes.Ldc_I4_1)
         il.Emit(OpCodes.Sub)   
         il.Emit(OpCodes.Stloc, local.LocalIndex)  
         il.Emit(OpCodes.Ldloc, local.LocalIndex)
         il.Emit(OpCodes.Ldc_I4_0)
         il.Emit(OpCodes.Cgt)
         il.Emit(OpCodes.Brtrue, label)
   and emitInvoke name args =
      for arg in args do il.Emit(OpCodes.Ldc_I4, arg)
      let t = typeof<Turtle>
      let mi = t.GetMethod(name)
      il.EmitCall(OpCodes.Call, mi, null)
   emitInvoke "Init" [||]  
   for command in program do emitCommand command
   emitInvoke "Show" [||]

/// Compiles program instructions to a .Net assembly
let compileTo name (program:command list) =
    /// Builder for assembly
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(name),
            AssemblyBuilderAccess.RunAndSave)
    /// Builder for module
    let moduleBuilder = 
        assemblyBuilder.DefineDynamicModule(name+".exe")
    /// Builder for type
    let typeBuilder =
        moduleBuilder.DefineType("Program", TypeAttributes.Public)
    /// Main method representing main routine
    let mainBuilder =
        typeBuilder.DefineMethod(
            "_Main", 
            MethodAttributes.Static ||| MethodAttributes.Public,
            typeof<Void>,
            [|typeof<string[]>|])
    // Set name of main method arguments
    let args = mainBuilder.DefineParameter(1, ParameterAttributes.None, "args")
    // IL generator for main method
    let il = mainBuilder.GetILGenerator()
    // Emit program instructions
    emitInstructions il program
    il.Emit(OpCodes.Ret)
    // Set main method as entry point
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore    
    assemblyBuilder.Save(name+".exe")

let codeBlock = Parser.parse code //[Repeat(3,[Forward 2; Left 10])]

System.IO.Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
do compileTo "MyProgram" codeBlock