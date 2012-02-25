with Ada.Text_IO; use Ada.Text_IO;

with Network.Config;
with Config;
with ProgramArguments;
procedure SimCtr is

   Configuration : Config.Config_Type;

begin
   ProgramArguments.Debug;

   Network.Config.LoadConfiguration
     (Configuration => Configuration);
   loop
      null;
   end loop;
end SimCtr;
