module AST =
   type distance = int
   type degrees = int
   type count = int
   type command =
      | Forward of distance
      | Turn of degrees      
      | Repeat of count * command list

open AST
open System

let getVar = 
   let varId = ref 0
   fun () -> incr varId; sprintf "_%d" !varId 

let rec emitCommand command =
      match command with
      | Forward n -> sprintf "forward(%d);" n  
      | Turn n -> sprintf "turn(%d);" n
      | Repeat(n,commands) ->
         let block = emitBlock commands
         String.Format("for({0}=0;{0}<{1};{0}++) {{\r\n {2}\r\n}}", getVar(), n, block); 
and emitBlock commands =
   String.concat "" [|for command in commands -> emitCommand(command)|]   

let program = [Repeat(36,[Forward 2;Turn 10])]
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

// set-random-position - Note: need to escape minus sign
function set_random_position() {
  x = Math.random() * width;
  y = Math.random() * height; 
  ctx.moveTo(x,y); 
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