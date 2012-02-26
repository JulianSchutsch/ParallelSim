with Config;
with Network.Config;
with Network.Streams;
with BSDSockets.Streams;
with ProgramArguments;
with SimElement;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure simele is

   Configuration : Config.Config_Type;

begin
   ProgramArguments.Debug;

   Network.Config.LoadConfiguration
     (Configuration => Configuration);

   Config.Debug
     (Item => Configuration);

   SimElement.Initialize
     (Configuration => Configuration);

   loop
      exit when SimElement.Process;
   end loop;

   SimElement.Finalize;
exception
   when E:others =>
      Put("Exception Name : " & Exception_Name(E));
      New_Line;
      Put("Traceback      :");
      New_Line;
      Put(Symbolic_TraceBack(E));
      New_Line;
      loop
         null;
      end loop;
end simele;
