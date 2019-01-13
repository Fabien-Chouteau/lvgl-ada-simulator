with Interfaces.C;    use Interfaces.C;
with SDL_SDL_h;       use SDL_SDL_h;
with SDL_SDL_video_h; use SDL_SDL_video_h;

with LV; use LV;
with LV.Color;
with LV.VDB;
with LV.HAL.Disp;     use LV.HAL.Disp;
with LV.HAL.Indev;     use LV.HAL.Indev;
with LV.Indev;
with LV.Objx;
with LV.Objx.Img;
with Ada.Text_IO;

with SDL_SDL_mouse_h;
with SDL_SDL_stdinc_h;
with SDL_SDL_events_h; use SDL_SDL_events_h;
with SDL_SDL_keysym_h; use SDL_SDL_keysym_h;

with GNAT.OS_Lib;

package body SDL_Display is

   Display : access SDL_Surface;

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

      if SDL_Init (SDL_INIT_VIDEO) < 0 then
         raise Program_Error with "SDL Video init failed";
         return;
      end if;

      Display := SDL_SetVideoMode (int (Width),
                                   int (Height),
                                   SDL_Pixel'Size,
                                   SDL_SWSURFACE);

      if Display = null then
         raise Program_Error with "Cannot create SDL display";
      end if;

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

   ----------
   -- Read --
   ----------

   function Read (Data : access indev_data_t) return u_Bool is
      X, Y : aliased Interfaces.C.int;
      State : SDL_SDL_stdinc_h.Uint8;
      Evt : aliased SDL_Event;
   begin
      while SDL_PollEvent (Evt'Access) /= 0 loop

         if Evt.c_type = SDL_KEYDOWN or else Evt.c_type = SDL_KEYUP then
            case Evt.key.keysym.sym is
            when SDLK_ESCAPE =>
               if Evt.c_type = SDL_KEYDOWN then
                  GNAT.OS_Lib.OS_Exit (0);
               end if;
            when others =>
               null;
            end case;
         end if;
      end loop;

      State := SDL_SDL_mouse_h.SDL_GetMouseState (X'Access, Y'Access);

      Data.Union.point := (LV.Int16_T (X), LV.Int16_T (Y));
      if (State and SDL_SDL_mouse_h.SDL_BUTTON_LEFT) /= 0 then
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
      Buffer : array (0 .. Int32_T (Display.w * Display.h - 1))
        of SDL_Pixel
          with Address => Display.pixels;

      Index : Natural := Color'First;
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

      for Y in Y1 .. Y2 loop
         for X in X1 .. X2 loop
            Buffer (Y * Int32_T (Width) + X) := LV.Color.Color_To16 (Color (Index));
            Index := Index + 1;
         end loop;
      end loop;

      SDL_UpdateRect (Display,
                      int (X1),
                      int (Y1),
                      unsigned (X2 - X1 + 1),
                      unsigned (Y2 - Y1 + 1));

      LV.VDB.Flush_Ready;
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
      Buffer : array (0 .. Int32_T (Display.w * Display.h - 1))
        of SDL_Pixel
          with Address => Display.pixels;

      C : constant Uint16_T := LV.Color.Color_To16 (Color);
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

      for Y in Y1 .. Y2 loop
         for X in X1 .. X2 loop
            Buffer (Y * Int32_T (Width) + X) := C;
         end loop;
      end loop;

      SDL_UpdateRect (Display,
                      int (X1),
                      int (Y1),
                      unsigned (X2 - X1 + 1),
                      unsigned (Y2 - Y1 + 1));
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
   begin
      return SDL_Pixel (SDL_MapRGB (Display.format,
                        unsigned_char (R),
                        unsigned_char (G),
                        unsigned_char (B)));
   end To_SDL_Color;

end SDL_Display;
