with LV;
with LV.Font;
with LV.Theme;
with LV.Tasks;
with LV.HAL.Tick;

with Test_Theme_1;
with SDL_HAL_800_480;

procedure Main is
begin
   LV.Init;

   SDL_HAL_800_480.Initialize;

   Test_Theme_1.Init;

   loop
      LV.Tasks.Handler;
      delay 0.005;
      lV.HAL.Tick.Inc (5);
   end loop;
end Main;
