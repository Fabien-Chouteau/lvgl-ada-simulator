
with Interfaces; use Interfaces;

with LV.Area;

generic

   Width  : Interfaces.Integer_16;
   Height : Interfaces.Integer_16;

package SDL_Display is

   Screen_Rect : constant LV.Area.Area_T := (0, 0, Width - 1, Height - 1);

   procedure Initialize;

   subtype SDL_Pixel is Unsigned_16;

   function To_SDL_Color (R, G, B : Unsigned_8) return SDL_Pixel;
end SDL_Display;
