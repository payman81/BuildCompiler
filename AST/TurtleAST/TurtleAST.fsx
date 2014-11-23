module TurtleAST

type distance = int
type degrees = int
type count = int
type command =
   | Forward of distance
   | Turn of degrees      
   | Repeat of count * command list