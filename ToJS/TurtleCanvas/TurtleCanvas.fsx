module AST =
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

open AST
open System

let getVar = 
   let varId = ref 0
   fun () -> incr varId; sprintf "_%d" !varId 

let rec emitCommand command =
      match command with
      | Forward arg -> sprintf "forward(%s);" (emitArg arg)  
      | Left arg -> sprintf "turn(-(%s));" (emitArg arg)
      | Right arg -> sprintf "turn(%s);" (emitArg arg)
      | Proc(name,params,commands)->
        sprintf "\r\nfunction %s(%s) {\r\n%s}" 
         name 
         (String.concat "," ``params``) 
         (emitBlock commands) 
         
      | Call(name,args)->
         let s = args |> List.map emitArg |> String.concat "+"
         name + "(" + s + ");"
      | Repeat(n,commands) ->
         let block = emitBlock commands
         String.Format("for({0}=0;{0}<{1};{0}++) {{\r\n {2}\r\n}}", getVar(), emitArg n, block); 
and emitArg arg = 
   match arg with
   | Number n -> sprintf "%d" n
   | Arg s -> s 
and emitBlock commands =
   String.concat "" [|for command in commands -> emitCommand(command)|]   

let program = [
    Proc("circle", [],
     [Repeat(Number 36,[Forward(Number 2);Right(Number 10)])])
    Repeat(Number 50, [SetRandomPosition;Call("circle",[])])
]
let generatedJS = emitBlock program

let html = 
   sprintf """<html>
<body>
<canvas id="myCanvas" width="400" height="400"
style="border:1px solid #000000;">
</canvas>
<script>
var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");
var width = 400;
var height = 400;
var x = width/2;
var y = height/2;
ctx.moveTo(x,y);
var a = 23.0;

function forward(n) {
  x += Math.cos((a*Math.PI)/180.0) * n;
  y += Math.sin((a*Math.PI)/180.0) * n;
  ctx.lineTo(x,y);
  ctx.stroke();
}
function turn(n) {
  a += n;
}

function set_random_position() {
  x = Math.random() * width;
  y = Math.random() * height; 
  ctx.moveTo(x,y); 
}

function square() {
    for (_i1 = 0; _i1 < 4; _i1++) {
        if (false) {
            forward(10);
            turn(90);
        } else {
            forward(50);
            turn(90);
        }
    }        
}

function flower() {
    for (_i2 = 0; _i2 < 36; _i2++) {
        turn(10);
        square();
    }
}

function garden(count) {
    for (_i3 = 0; _i3 < count; _i3++) {
        set_random_position();
        flower();
    }
}

    // Generated JS
    %s

</script>
</body>
</html>""" generatedJS

open System.IO

let path = Path.Combine(__SOURCE_DIRECTORY__, "TurtleGen.html")
File.WriteAllText(path, html)

path |> System.Diagnostics.Process.Start 