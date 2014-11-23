using System;
using System.Drawing;
using System.Windows.Forms;

public static class Turtle
{
   const int Width = 640;
   const int Height = 480;

   private static double X = Width / 2;
   private static double Y = Height / 2;
   private static int Angle = -90;

   public static Image Image;
   public static Pen Pen;

   public static void Init()
   {
      Image = new Bitmap(Width, Height);
      Pen = new Pen(Color.Red);
   }

   public static void Show()
   {
      var form = new Form { Text = "Turtle", Width = Width, Height = Height };
      var pictureBox = new PictureBox { Dock = DockStyle.Fill, Image = Image };
      form.Controls.Add(pictureBox);
      form.ShowDialog();
   }

   public static void Run(Action action)
   {
      Init();
      action();
      Show();
   }

   public static void Forward(int n)
   {
      var radians = Angle * Math.PI / 180;
      var x2 = X + n * Math.Cos(radians);
      var y2 = Y + n * Math.Sin(radians);
      using (var graphics = Graphics.FromImage(Image))
      {
         graphics.DrawLine(Pen, (float)X, (float)Y, (float)x2, (float)y2);
      }
      X = x2;
      Y = y2;
   }

   public static void Turn(int n)
   {
      Angle += n;
   }
}