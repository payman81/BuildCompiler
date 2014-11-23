#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml.dll"
#r "UIAutomationTypes.dll"
#r "WindowsBase.dll"

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
    
[<AutoOpen>]
module AST =    
    type reference = string
    type distance = int
    type degrees = int
    type instruction =
        | Forward of distance
        | Left of degrees
        | Right of degrees
        | Repeat of int * instruction list    
        | PenDown 
        | PenUp
        | PenColor of Color
        | PenWidth of distance
        | ClearScreen
        | ShowTurtle
        | HideTurtle
   
[<AutoOpen>]
module Lang =
    let forward n = Forward n
    let fd = forward
    let left n = Left n
    let lt = left
    let right n = Right n
    let rt = right
    let repeat n (instructions:instruction list) =
        Repeat(n,instructions)
    let once (instructions:instruction list) =
        Repeat(1,instructions)
    let run = once
    let output x = x
    let pencolor color = PenColor color
    let penup = PenUp
    let pendown = PenDown
    let penwidth n = PenWidth n
    let clearscreen = ClearScreen
    let cs = clearscreen
    let hideturtle = HideTurtle
    let ht = hideturtle
    let showturtle = ShowTurtle
    let st = showturtle
   
[<AutoOpen>]
module Colors =
    let red = Colors.Red
    let green = Colors.Green
    let blue = Colors.Blue
    let white = Colors.White
    let black = Colors.Black
    let gray = Colors.Gray
    let yellow = Colors.Yellow
    let orange = Colors.Orange
    let brown = Colors.Brown
    let cyan = Colors.Cyan
    let magenta = Colors.Magenta
    let purple = Colors.Purple

type Turtle private () =
    inherit UserControl(Width = 512.0, Height = 400.0)    
    let screen = Canvas(Background = SolidColorBrush Colors.Black)
    do  base.Content <- screen
    let mutable penColor = white
    let mutable penWidth = 1.0
    let mutable isPenDown = true
    let mutable x, y = 0.0, 0.0
    let mutable a = 270
    let addLine (canvas:Canvas) x' y' =
        let line = Line(X1=x,Y1=y,X2=x',Y2=y')
        line.StrokeThickness <- penWidth
        line.Stroke <- SolidColorBrush penColor 
        canvas.Children.Add line |> ignore
    let clearLines () = 
        screen.Children.Clear()        
    let turtle = Canvas()
    let rotation = RotateTransform(Angle=float a)
    do  turtle.RenderTransform <- rotation    
    do  screen.Children.Add turtle |> ignore
    let rec execute canvas = function
        | Forward n ->
            let n = float n
            let r = float a * Math.PI / 180.0
            let dx, dy = float n * cos(r), float n * sin(r)
            let x', y' = x + dx, y + dy
            if isPenDown then addLine canvas x' y'
            x <- x'
            y <- y'
        | Left n -> 
            a <- a - n
            rotation.Angle <- float a
        | Right n -> 
            a <- a + n
            rotation.Angle <- float a
        | Repeat(n,instructions) ->
            for i = 1 to n do
                instructions |> List.iter (execute canvas)
        | PenDown -> isPenDown <- true
        | PenUp -> isPenDown <- false
        | PenColor color -> penColor <- color
        | PenWidth width -> penWidth <- float width
        | ClearScreen -> clearLines ()
        | ShowTurtle ->
            turtle.Visibility <- Visibility.Visible
        | HideTurtle ->
            turtle.Visibility <- Visibility.Collapsed
    let drawTurtle () =
        [penup; forward 5; pendown; 
         right 150;  forward 10; 
         right 120; forward 10; 
         right 120; forward 10; 
         right 150; penup; forward 5; right 180; pendown]
        |> List.iter (execute turtle)    
    do  drawTurtle ()
        x <- base.Width/2.0
        y <- base.Height/2.0
    do  Canvas.SetLeft(turtle,x)
        Canvas.SetTop(turtle,y)
    static let control = lazy(Turtle ())    
    member private this.Execute instruction = 
        execute screen instruction
    /// Turtle screen
    static member Screen = control.Force()    
    /// Runs turtle instruction
    static member Run (instruction:instruction) =
        control.Force().Execute instruction
    /// Runs sequence of turtle instructions
    static member Run (instructions:instruction seq) =
        control.Force().Dispatcher.Invoke(fun x ->
            instructions |> Seq.iter (control.Force().Execute)
        )
    
[<STAThread>]
do  let win = Window(Content=Turtle.Screen, Width=512., Height=400.)
    win.Show()

pencolor red
|> Turtle.Run 
    
[for x in 0..99 -> once [fd (x * 3); rt 121]]
|> Turtle.Run
      
//hideturtle |> Turtle.Run