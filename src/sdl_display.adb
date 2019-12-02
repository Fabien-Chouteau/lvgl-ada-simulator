with Ada.Unchecked_Conversion;
with SDL.Video.Pixels;
with System;
with Interfaces.C;    use Interfaces.C;

with SDL;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes; use SDL.Video.Palettes;
with SDL.Video.Pixel_Formats; use SDL.Video.Pixel_Formats;
with SDL.Video.Textures; use SDL.Video.Textures;
with SDL.Video.Textures.Makers;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

with SDL.Events;           use SDL.Events;
with SDL.Events.Events;    use SDL.Events.Events;
with SDL.Events.Keyboards; use SDL.Events.Keyboards;
with SDL.Events.Mice;      use SDL.Events.Mice;
with SDL.Inputs.Mice;      use SDL.Inputs.Mice;

with LV; use LV;
with LV.Color;
with LV.VDB;
with LV.HAL.Disp;     use LV.HAL.Disp;
with LV.HAL.Indev;     use LV.HAL.Indev;
with LV.Indev;
with LV.Objx;
with LV.Objx.Img;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body SDL_Display is

   W          : SDL.Video.Windows.Window;
   Renderer   : SDL.Video.Renderers.Renderer;
   Texture    : SDL.Video.Textures.Texture;
   SDL_Pixels : System.Address;

   type Texture_1D_Array is array (Int32_T range <>)
     of aliased SDL_Pixel;

   procedure Lock is new SDL.Video.Textures.Lock
     (Pixel_Pointer_Type => System.Address);

   function To_Address is
     new Ada.Unchecked_Conversion
       (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
        Target => System.Address);

   procedure Update;

   LV_Disp_Drv : aliased Disp_Drv_T;
   LV_Disp : Disp_T;

   LV_Indev_Drv : aliased Indev_Drv_T;
   LV_Indev : Indev_T;
   Cursor_Obj : LV.Objx.Img.Instance;

   function Read (Data : access indev_data_t) return u_Bool
     with Convention => C;

   procedure Disp_Flush
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : access constant Color_Array)
   with Convention => C;

   procedure Disp_Fill
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : LV.Color.Color_T)
   with Convention => C;

   procedure Disp_Map
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : access constant Color_Array)
   with Convention => C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Mouse_Cursor_Icon : Integer;
      pragma Import (C, Mouse_Cursor_Icon, "mouse_cursor_icon");
   begin

      if not SDL.Initialise (Flags => SDL.Enable_Screen) then
         raise Program_Error with "SDL Video init failed";
      end if;

      SDL.Video.Windows.Makers.Create
        (W, "LVGL-Ada Example",
         0,
         0,
         Integer (Width),
         Integer (Height),
         Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create
        (Tex      => Texture,
         Renderer => Renderer,
         Format   => SDL.Video.Pixel_Formats.Pixel_Format_RGB_565,
         Kind     => SDL.Video.Textures.Streaming,
         Size     => (Integer (Width),
                      Integer (Height)));
      LV.HAL.Disp.Init_Drv (LV_Disp_Drv'Access);

      LV_Disp_Drv.disp_flush := disp_flush'Access;
      LV_Disp_Drv.disp_fill := disp_fill'Access;
      LV_Disp_Drv.disp_map := disp_map'Access;

      LV_Disp := LV.HAL.Disp.register (LV_Disp_Drv'Access);
      LV.HAL.Disp.set_active (LV_Disp);


      LV.HAL.Indev.Init_Drv (LV_Indev_Drv'Access);
      LV_Indev_Drv.read := Read'Access;
      LV_Indev_Drv.c_type := LV.HAL.Indev.TYPE_POINTER;
      LV_Indev := LV.HAL.Indev.register (LV_Indev_Drv'Access);

      Cursor_Obj := LV.Objx.Img.Create (lv.Objx.Scr_Act, LV.Objx.No_Obj);
      LV.Objx.Img.Set_Src (Cursor_Obj, Mouse_Cursor_Icon'Address);
      LV.Indev.set_cursor (LV_Indev, Cursor_Obj);
   end Initialize;

   ------------
   -- Update --
   ------------

   procedure Update is
      Width  : constant Natural := Texture.Get_Size.Width;
      Height : constant Natural := Texture.Get_Size.Height;
   begin

      Renderer.Clear;
      Renderer.Copy (Texture, To => (0,
                                     0,
                                     int (Width),
                                     int (Height)));
      Renderer.Present;
   end Update;

   ----------
   -- Read --
   ----------

   function Read (Data : access indev_data_t) return u_Bool is
      Event   : SDL.Events.Events.Events;

      Mouse_Button_Masks : SDL.Events.Mice.Button_Masks;
      X, Y : SDL.Events.Mice.Movement_Values;
   begin
      while SDL.Events.Events.Poll (Event) loop
         if Event.Common.Event_Type in Key_Down | Key_Up then

            case Event.Keyboard.Key_Sym.Scan_Code is
            when Scan_Code_Escape =>
               GNAT.OS_Lib.OS_Exit (0);
            when others =>
               null;
            end case;
         end if;
      end loop;



      Mouse_Button_Masks := SDL.Inputs.Mice.Get_State (X, Y);

      Data.Union.point := (LV.Int16_T (X), LV.Int16_T (Y));
      if (Mouse_Button_Masks and Left_Mask) /= 0 then
         Data.state := LV.HAL.Indev.STATE_PR;
      else
         Data.state := LV.HAL.Indev.STATE_REL;
      end if;

      --  Return false because the points are not buffered, so no more data to
      --  be read.
      return 0;
   end Read;

   ----------------
   -- Disp_Flush --
   ----------------

   procedure Disp_Flush
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : access constant Color_Array)
   is

      Width  : constant Natural := Texture.Get_Size.Width;
      Height : constant Natural := Texture.Get_Size.Height;
   begin
      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Width - 1)
        or else
         Y1 > Int32_T (Height - 1)
      then
         LV.VDB.Flush_Ready;
         return;
      end if;

      Lock (Texture, SDL_Pixels);

      declare
         Actual_Pixels : Texture_1D_Array (0 .. Int32_T (Width * Height - 1))
           with
             Address => SDL_Pixels;

         Index : Natural := Color'First;
      begin

         for Y in Y1 .. Y2 loop
            for X in X1 .. X2 loop
               Actual_Pixels (Y * Int32_T (Width) + X)
                 := LV.Color.Color_To16 (Color (Index));
               Index := Index + 1;
            end loop;
         end loop;
      end;

      LV.VDB.Flush_Ready;
      Texture.Unlock;
      Update;
   end Disp_Flush;

   ---------------
   -- Disp_Fill --
   ---------------

   procedure Disp_Fill
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : LV.Color.Color_T)
   is

      Width  : constant Natural := Texture.Get_Size.Width;
      Height : constant Natural := Texture.Get_Size.Height;
      C      : constant Uint16_T := LV.Color.Color_To16 (Color);
   begin
      if X2 < 0
        or else
         Y2 < 0
        or else
         X1 > Int32_T (Width - 1)
        or else
         Y1 > Int32_T (Height - 1)
      then
         LV.VDB.Flush_Ready;
         return;
      end if;

      Lock (Texture, SDL_Pixels);

      declare
         Actual_Pixels : Texture_1D_Array (0 .. Int32_T (Width * Height - 1))
           with
             Address => SDL_Pixels;
      begin

         for Y in Y1 .. Y2 loop
            for X in X1 .. X2 loop
               Actual_Pixels (Y * Int32_T (Width) + X) := C;
            end loop;
         end loop;
      end;

      LV.VDB.Flush_Ready;
      Texture.Unlock;
      Update;
   end Disp_Fill;

   --------------
   -- Disp_Map --
   --------------

   procedure Disp_Map
     (x1    : int32_t;
      y1    : int32_t;
      x2    : int32_t;
      y2    : int32_t;
      Color : access constant Color_Array)
   is
   begin
      Ada.Text_IO.Put_Line ("X1:" & X1'Img);
      Ada.Text_IO.Put_Line ("Y1:" & Y1'Img);
      Ada.Text_IO.Put_Line ("X2:" & X2'Img);
      Ada.Text_IO.Put_Line ("Y2:" & Y2'Img);
      Ada.Text_IO.Put_Line ("Length:" & Color'Length'Img);
      Ada.Text_IO.Put_Line ("First:" & Color'First'Img);
   end Disp_Map;

   ------------------
   -- To_SDL_Color --
   ------------------

   function To_SDL_Color (R, G, B : Unsigned_8) return SDL_Pixel is
      RB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (R), 3) and 16#1F#;
      GB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (G), 2) and 16#3F#;
      BB : constant Unsigned_16 :=
        Shift_Right (Unsigned_16 (B), 3) and 16#1F#;
   begin
      return (Shift_Left (RB, 11) or Shift_Left (GB, 5) or BB);
   end To_SDL_Color;

end SDL_Display;
