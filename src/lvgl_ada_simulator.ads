with Lv.Hal.Indev;

package LVGL_Ada_Simulator is

   procedure Start (Title : String; Refresh_Rate : Positive);

   procedure Add_Pointer (With_Cursor : Boolean := False);

   procedure Add_Keyboard;

   function Keyboard_Indev return Lv.Hal.Indev.Indev_T;

   procedure Take_Screenshot (Path : String);
   -- The format of the image is automatically deduced from the extension. The
   -- supported image formats are bmp, png, tga and jpg. The destination file
   -- is overwritten if it already exists.

end LVGL_Ada_Simulator;
