with System;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Containers.Doubly_Linked_Lists;

with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Sf.Window.VideoMode; use Sf.Window.VideoMode;
with Sf.Window.Keyboard;
with Sf.Graphics; use Sf.Graphics;
with Sf.Graphics.Rect;
with Sf.Graphics.Sprite; use Sf.Graphics.Sprite;
with Sf.Graphics.Texture; use Sf.Graphics.Texture;
with Sf.Graphics.RenderTexture; use Sf.Graphics.RenderTexture;
with Sf.Graphics.View; use Sf.Graphics.View;
with Sf.Graphics.RenderWindow; use Sf.Graphics.RenderWindow;
with Sf.Graphics.Image;
with Sf.Window.Window; use Sf.Window.Window;
with Sf.Window.Event; use Sf.Window.Event;
with Sf.Window.Mouse; use Sf.Window.Mouse;
with Sf; use Sf;

with Sf.System.Vector2; use Sf.System.Vector2;

with Interfaces;   use Interfaces;

with Lvgl_Ada_Config;
with Lv; use Lv;
with Lv.Vdb;
with Lv.Color;
with Lv.Area;
with Lv.Hal.Disp;     use Lv.Hal.Disp;
with Lv.Hal.Indev;     use Lv.Hal.Indev;
with Lv.Objx;
with Lv.Objx.Img;
with Lv.Indev;


package body LVGL_Ada_Simulator is

   Screen_Width : constant := Lvgl_Ada_Config.Horizontal_Resolution;
   Screen_Height : constant := Lvgl_Ada_Config.Vertical_Resolution;

   Frame_Buffer : array (0 .. (Screen_Width * Screen_Height * 4) - 1) of
     aliased Sf.sfUint8
       := (others => 100);

   Port : Sf.Graphics.Rect.sfFloatRect := (0.0, 0.0, 1.0, 1.0);
   Win_Width, Win_Height : sfUint32 := 1;

   Title : Ada.Strings.Unbounded.Unbounded_String;
   Refresh_Rate : Positive := 60;

   -- LVGL --

   package LVGL_Event_Queue is

      package LVGL_Event_List
      is new Ada.Containers.Doubly_Linked_Lists (Indev_Data_T);

      protected type Queue is
         procedure Push (Elt : Indev_Data_T);
         procedure Pop (Elt : out Indev_Data_T; Success : out Boolean);
      private
         List : LVGL_Event_List.List;
      end Queue;
   end LVGL_Event_Queue;

   package body LVGL_Event_Queue is

      protected body Queue is
         procedure Push (Elt : Indev_Data_T) is
         begin
            List.Append (Elt);
         end Push;

         procedure Pop (Elt : out Indev_Data_T; Success : out Boolean) is
         begin
            if List.Is_Empty then
               Success := False;
            else
               Success := True;
               Elt := List.First_Element;
               List.Delete_First;
            end if;
         end Pop;
      end Queue;
   end LVGL_Event_Queue;

   LV_Disp_Drv : aliased Disp_Drv_T;
   LV_Disp : Disp_T;

   Pointer_Enabled      : Boolean := False;
   LV_Indev_Pointer_Drv : aliased Indev_Drv_T;
   LV_Indev_Pointer     : Indev_T;
   Cursor_Obj           : Lv.Objx.Img.Instance;
   Pointer_Queue        : LVGL_Event_Queue.Queue;
   In_Pointer_Pos       : Lv.Area.Point_T := (1, 1);
   In_Pointer_State     : Lv.Hal.Indev.Indev_State_T := Lv.Hal.Indev.State_Rel;
   Out_Pointer_Pos      : Lv.Area.Point_T := (1, 1);
   Out_Pointer_State    : Lv.Hal.Indev.Indev_State_T := Lv.Hal.Indev.State_Rel;

   Keypad_Enabled      : Boolean := False;
   LV_Indev_Keypad_Drv : aliased Indev_Drv_T;
   LV_Indev_Keypad     : Indev_T;
   Keypad_Queue        : LVGL_Event_Queue.Queue;

   function Read_Pointer (Data : access Indev_Data_T) return U_Bool
     with Convention => C;

   function Read_Keypad (Data : access Indev_Data_T) return U_Bool
     with Convention => C;

   procedure Disp_Flush
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   with Convention => C;

   procedure Disp_Fill
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : Lv.Color.Color_T)
   with Convention => C;

   procedure Disp_Map
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   with Convention => C;

   ------------------
   -- To_LVGL_Code --
   ------------------

   function To_LVGL_Code (Key : Sf.Window.Event.sfKeyEvent)
                          return Lv.Uint32_T
   is
      use Sf.Window.Keyboard;
   begin
      case Key.code is
         when sfKeyUp     => return Lv.LV_KEY_UP;
         when sfKeyDown   => return Lv.LV_KEY_DOWN;
         when sfKeyRight  => return Lv.LV_KEY_RIGHT;
         when sfKeyLeft   => return Lv.LV_KEY_LEFT;
         when sfKeyEscape => return Lv.LV_KEY_ESC;
         when sfKeyDelete => return Lv.LV_KEY_DEL;
         when sfKeyBack   => return Lv.LV_KEY_BACKSPACE;
         when sfKeyEnter  => return Lv.LV_KEY_ENTER;
         when sfKeyHome   => return LV.LV_KEY_HOME;
         when sfKeyEnd    => return LV.LV_KEY_END;
         when sfKeySpace  => return Character'Enum_Rep (' ');

         when sfKeyNum0 .. sfKeyNum9 =>
            return Character'Enum_Rep ('0') +
              Lv.Uint32_T (Key.code - sfKeyNum0);

         when sfKeyA .. sfKeyZ =>
            if Key.shift then
               return Character'Enum_Rep ('A') +
                 Lv.Uint32_T (Key.code - sfKeyA);
            else
               return Character'Enum_Rep ('a') +
                 Lv.Uint32_T (Key.code - sfKeyA);
            end if;

         when others =>
            return 1;
      end case;
   end To_LVGL_Code;

   --------------
   -- Viewport --
   --------------

   function Viewport (View : Sf.Graphics.sfView_Ptr;
                      Width, Height : sfUint32)
                      return Sf.Graphics.Rect.sfFloatRect
   is
      Win_Ratio  : constant Float := Float (Width) / Float (Height);
      View_Ratio : constant Float := getSize (View).x / getSize (View).y;
      Size_X : Float := 1.0;
      Size_Y : Float := 1.0;
      Pos_X : Float := 0.0;
      Pos_Y : Float := 0.0;

   begin
      if Win_Ratio < View_Ratio then
         Size_Y := Win_Ratio / View_Ratio;
         Pos_Y := (1.0 - Size_Y) / 2.0;
      else
         Size_X := View_Ratio / Win_Ratio;
         Pos_X := (1.0 - Size_X) / 2.0;
      end if;

      return (Pos_X, Pos_Y, Size_X, Size_Y);
   end Viewport;

   --------------
   -- Set_View --
   --------------

   procedure Set_View (View : Sf.Graphics.sfView_Ptr;
                       Port : Sf.Graphics.Rect.sfFloatRect)
   is
   begin
      setViewport (View, Port);
   end Set_View;

   ----------------------------
   -- Screenshot_From_Window --
   ----------------------------

   procedure Screenshot_From_Window (Window : sfRenderWindow_Ptr;
                                     Path : String)
   is
      Size : constant Sf.System.Vector2.sfVector2u :=
        Sf.Graphics.RenderWindow.getSize (Window);

      Tex : constant Sf.Graphics.sfTexture_Ptr :=
        Sf.Graphics.Texture.create (Size.x, Size.y);

      Img : Sf.Graphics.sfImage_Ptr;
   begin
      Sf.Graphics.Texture.updateFromRenderWindow
        (Tex, Window, 0, 0);

      Img := Sf.Graphics.Texture.copyToImage (Tex);

      if not Sf.Graphics.Image.saveToFile (Img, Path) then
         raise Program_Error with "Cannot save screenshot...";
      end if;

      Sf.Graphics.Image.destroy (Img);
      Sf.Graphics.Texture.destroy (Tex);
   end Screenshot_From_Window;

   task Periodic_Update is
      entry Start;
      entry Take_Screenshot (Path : String);
   end Periodic_Update;

   ---------------------
   -- Periodic_Update --
   ---------------------

   task body Periodic_Update is
      BG_Width : constant := Screen_Width;
      BG_Height : constant := Screen_Height;
      Mode   : constant Sf.Window.VideoMode.sfVideoMode :=
        (BG_Width, BG_Height, 32);

      Params : constant sfContextSettings := sfDefaultContextSettings;
      Window : Sf.Graphics.sfRenderWindow_Ptr;
      Framebuffer_Texture : Sf.Graphics.sfTexture_Ptr;
      Render_Texture : Sf.Graphics.sfRenderTexture_Ptr;
      Screen_Sprite : Sf.Graphics.sfSprite_Ptr;
      Sprite_Right : Sf.Graphics.sfSprite_Ptr;
      Letter_Box_View : Sf.Graphics.sfView_Ptr;
      Event   : sfEvent;

      Period : constant Time_Span := Milliseconds (1000 / Refresh_Rate);
      Next_Release : Time := Clock + Period;

      Screen_Scale : constant := 1.0; -- 296.0 / Float (Screen_Width);
      Screen_Offset : constant sfVector2f := (0.0, 0.0);
   begin

      accept Start;

      Framebuffer_Texture := create (Screen_Width, Screen_Height);
      if Framebuffer_Texture = null then
         Put_Line ("Failed to create screen texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Render_Texture := create (Screen_Width, Screen_Height, False);
      if Render_Texture = null then
         Put_Line ("Could not create render texture");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Screen_Sprite := create;
      if Screen_Sprite = null then
         Put_Line ("Could not create screen sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (Screen_Sprite, getTexture (Render_Texture));
      scale (Screen_Sprite, (Screen_Scale, Screen_Scale));
      setPosition (Screen_Sprite, Screen_Offset);

      Sprite_Right := create;
      if Sprite_Right = null then
         Put_Line ("Could not create sprite");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setTexture (Sprite_Right, Framebuffer_Texture);

      Window := create (Mode,
                        Ada.Strings.Unbounded.To_String (Title),
                        sfResize or sfClose,
                        Params);

      if Window = null then
         Put_Line ("Failed to create window");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      setVerticalSyncEnabled (Window, sfFalse);
      setVisible (Window, sfTrue);

      Letter_Box_View := create;
      if Letter_Box_View = null then
         Put_Line ("Failed to create view");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
      setSize (Letter_Box_View, (Float (BG_Width), Float (BG_Height)));
      setCenter (Letter_Box_View,
                 (Float (BG_Width) / 2.0, Float (BG_Height) / 2.0));

      Win_Width := getSize (Window).x;
      Win_Height := getSize (Window).y;
      Port := Viewport (Letter_Box_View, Win_Width, Win_Height);
      Set_View (Letter_Box_View, Port);

      loop
         delay until Next_Release;
         Next_Release := Next_Release + Period;

         while pollEvent (Window, Event) loop

            case Event.eventType is
               when sfEvtClosed =>
                  close (Window);
                  Put_Line ("Attempting to close");
                  GNAT.OS_Lib.OS_Exit (0);

               when sfEvtResized =>

                  Win_Width := getSize (Window).x;
                  Win_Height := getSize (Window).y;
                  Port := Viewport (Letter_Box_View, Win_Width, Win_Height);

                  Set_View (Letter_Box_View, Port);

               when sfEvtMouseMoved =>
                  In_Pointer_Pos := (Int16_T (Event.mouseMove.x),
                                     Int16_T (Event.mouseMove.y));

                  if Pointer_Enabled then
                     Pointer_Queue.Push
                       ((Union => (Discr => 0,
                                   Point => In_Pointer_Pos),
                         User_Data => System.Null_Address,
                         State => In_Pointer_State));
                  end if;

               when sfEvtMouseButtonPressed =>
                  if Event.mouseButton.button = sfMouseLeft then
                     In_Pointer_State := Lv.Hal.Indev.State_Pr;

                     if Pointer_Enabled then
                        Pointer_Queue.Push
                          ((Union => (Discr => 0,
                                      Point => In_Pointer_Pos),
                            User_Data => System.Null_Address,
                            State => In_Pointer_State));
                     end if;

                  end if;

               when sfEvtMouseButtonReleased =>
                  if Event.mouseButton.button = sfMouseLeft then
                     In_Pointer_State := Lv.Hal.Indev.State_Rel;

                     if Pointer_Enabled then
                        Pointer_Queue.Push
                          ((Union => (Discr => 0,
                                      Point => In_Pointer_Pos),
                            User_Data => System.Null_Address,
                            State => In_Pointer_State));
                     end if;
                  end if;

               when sfEvtKeyPressed | sfEvtKeyReleased =>
                  if Keypad_Enabled then
                     Keypad_Queue.Push
                       ((Union => (Discr => 1,
                                   Key => To_LVGL_Code (Event.key)),
                         User_Data => System.Null_Address,
                         State => (if Event.eventType = sfEvtKeyPressed
                                   then Lv.Hal.Indev.State_Pr
                                   else Lv.Hal.Indev.State_Rel)));

                  end if;
               when others =>
                  null;
            end case;

         end loop;

         updateFromPixels (texture => Framebuffer_Texture,
                           pixels  => Frame_Buffer (Frame_Buffer'First)'Access,
                           width   => Screen_Width,
                           height  => Screen_Height,
                           x       => 0,
                           y       => 0);

         setPosition (Sprite_Right, (0.0, 0.0));

         drawSprite (Render_Texture, Sprite_Right);
         display (Render_Texture);

         clear (Window);
         drawSprite (Window, Screen_Sprite);

         select
            accept Take_Screenshot (Path : String) do
               Screenshot_From_Window (Window, Path);
            end Take_Screenshot;
         else
            null;
         end select;

         setView (Window, Letter_Box_View);
         display (Window);

      end loop;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (1);
   end Periodic_Update;

   -----------
   -- Start --
   -----------

   procedure Start (Title : String; Refresh_Rate : Positive) is
   begin
      LVGL_Ada_Simulator.Title :=
        Ada.Strings.Unbounded.To_Unbounded_String (Title);

      LVGL_Ada_Simulator.Refresh_Rate := Refresh_Rate;

      Periodic_Update.Start;

      Lv.Hal.Disp.Init_Drv (LV_Disp_Drv'Access);

      LV_Disp_Drv.Disp_Flush := Disp_Flush'Access;
      LV_Disp_Drv.Disp_Fill := Disp_Fill'Access;
      LV_Disp_Drv.Disp_Map := Disp_Map'Access;

      LV_Disp := Lv.Hal.Disp.Register (LV_Disp_Drv'Access);
      Lv.Hal.Disp.Set_Active (LV_Disp);
   end Start;

   ----------------
   -- Add_Cursor --
   ----------------

   procedure Add_Pointer (With_Cursor : Boolean := False) is
      Mouse_Cursor_Icon : Integer;
      pragma Import (C, Mouse_Cursor_Icon, "lvgl_ada_sim_mouse_cursor_icon");
   begin

      Lv.Hal.Indev.Init_Drv (LV_Indev_Pointer_Drv'Access);
      LV_Indev_Pointer_Drv.Read := Read_Pointer'Access;
      LV_Indev_Pointer_Drv.C_Type := Lv.Hal.Indev.Type_Pointer;
      LV_Indev_Pointer := Lv.Hal.Indev.Register (LV_Indev_Pointer_Drv'Access);

      if With_Cursor then
         Cursor_Obj := Lv.Objx.Img.Create (Lv.Objx.Scr_Act, Lv.Objx.No_Obj);
         Lv.Objx.Img.Set_Src (Cursor_Obj, Mouse_Cursor_Icon'Address);
         Lv.Indev.Set_Cursor (LV_Indev_Pointer, Cursor_Obj);
      end if;

      Pointer_Enabled := True;
   end Add_Pointer;

   ------------------
   -- Add_Keyboard --
   ------------------

   procedure Add_Keyboard is
   begin
      Lv.Hal.Indev.Init_Drv (LV_Indev_Keypad_Drv'Access);
      LV_Indev_Keypad_Drv.Read := Read_Keypad'Access;
      LV_Indev_Keypad_Drv.C_Type := Lv.Hal.Indev.Type_Keypad;
      LV_Indev_Keypad := Lv.Hal.Indev.Register (LV_Indev_Keypad_Drv'Access);
      Keypad_Enabled := True;
   end Add_Keyboard;

   --------------------
   -- Keyboard_Indev --
   --------------------

   function Keyboard_Indev return Lv.Hal.Indev.Indev_T
   is (LV_Indev_Keypad);

   ---------------------
   -- Take_Screenshot --
   ---------------------

   procedure Take_Screenshot (Path : String) is
   begin
      Periodic_Update.Take_Screenshot (Path);
   end Take_Screenshot;

   ------------------
   -- Read_Pointer --
   ------------------

   function Read_Pointer (Data : access Indev_Data_T) return U_Bool is
      Left   : constant Int16_T := Int16_T (Port.left * Float (Win_Width));
      Top    : constant Int16_T := Int16_T (Port.top * Float (Win_Height));
      Width  : constant Int16_T := Int16_T (Port.width * Float (Win_Width));
      Height : constant Int16_T := Int16_T (Port.height * Float (Win_Height));

      LV_Screen_Ratio_Y : constant Float :=
        Float (Screen_Height) / Float (Win_Height);

      LV_Screen_Ratio_X : constant Float :=
        Float (Screen_Width) / Float (Win_Width);

      Evt_In : Indev_Data_T;
      Scaled : Lv.Area.Point_T;
      Success : Boolean;
   begin

      Pointer_Queue.Pop (Evt_In, Success);
      if Success then
         Out_Pointer_Pos := Evt_In.Union.Point;
         Out_Pointer_State := Evt_In.State;
      end if;

      Scaled     := Out_Pointer_Pos;
      Data.State := Out_Pointer_State;

      if Scaled.X < Left then
         Scaled.X := 0;
      else
         Scaled.X := Scaled.X - Left;
      end if;

      if Scaled.Y < Top then
         Scaled.Y := 0;
      else
         Scaled.Y := Scaled.Y - Top;
      end if;

      Scaled.X := Int16_T (Float (Scaled.X) * LV_Screen_Ratio_X / Port.width);
      Scaled.Y := Int16_T (Float (Scaled.Y) * LV_Screen_Ratio_Y/ Port.height);

      Data.Union.Point := Scaled;
      return (if Success then 1 else 0);
   end Read_Pointer;

   -----------------
   -- Read_Keypad --
   -----------------

   function Read_Keypad (Data : access Indev_Data_T) return U_Bool is
      Success : Boolean;
   begin
      Keypad_Queue.Pop (Data.all, Success);
      return (if Success then 1 else 0);
   end Read_Keypad;

   ----------------
   -- Disp_Flush --
   ----------------

   procedure Disp_Flush
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   is
   begin
      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Screen_Width - 1)
        or else
         Y1 > Int32_T (Screen_Height - 1)
      then
         Lv.Vdb.Flush_Ready;
         return;
      end if;

      declare
         FB : array (Int32_T range 0 .. (Screen_Width * Screen_Height) - 1)
           of Uint32_T
           with Address => Frame_Buffer'Address;

         Index : Natural := Color'First;
      begin

         for Y in Y1 .. Y2 loop
            for X in X1 .. X2 loop
               FB (Y * Screen_Width + X)
                 := Lv.Color.Color_To32 (Color (Index));
               Index := Index + 1;
            end loop;
         end loop;
      end;

      Lv.Vdb.Flush_Ready;
   end Disp_Flush;

   ---------------
   -- Disp_Fill --
   ---------------

   procedure Disp_Fill
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : Lv.Color.Color_T)
   is
      C : constant Uint32_T := Lv.Color.Color_To32 (Color);
   begin
      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Screen_Width - 1)
        or else
         Y1 > Int32_T (Screen_Height - 1)
      then
         Lv.Vdb.Flush_Ready;
         return;
      end if;

      declare
         FB : array (Int32_T range 0 .. (Screen_Width * Screen_Height) - 1)
           of Uint32_T
           with Address => Frame_Buffer'Address;
      begin

         for Y in Y1 .. Y2 loop
            for X in X1 .. X2 loop
               FB (Y * Screen_Width + X) := C;
            end loop;
         end loop;
      end;

      Lv.Vdb.Flush_Ready;
   end Disp_Fill;

   --------------
   -- Disp_Map --
   --------------

   procedure Disp_Map
     (X1    : Int32_T;
      Y1    : Int32_T;
      X2    : Int32_T;
      Y2    : Int32_T;
      Color : access constant Color_Array)
   is
   begin
      --  Put_Line ("Disp_Map");
      --  Ada.Text_IO.Put_Line ("X1:" & X1'Img);
      --  Ada.Text_IO.Put_Line ("Y1:" & Y1'Img);
      --  Ada.Text_IO.Put_Line ("X2:" & X2'Img);
      --  Ada.Text_IO.Put_Line ("Y2:" & Y2'Img);
      --  Ada.Text_IO.Put_Line ("Length:" & Color'Length'Img);
      --  Ada.Text_IO.Put_Line ("First:" & Color'First'Img);
      null;
   end Disp_Map;

end LVGL_Ada_Simulator;
