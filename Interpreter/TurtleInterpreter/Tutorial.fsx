#load "AST.fs"
#load "Interpreter.fsx"

execute [Repeat(36,[Forward 10;Turn 10])]

execute [Pen Blue;Repeat(10,[Turn 36;Repeat(5,[Forward 54;Turn 72])])]