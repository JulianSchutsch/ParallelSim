with Config;
with Network.Config;

with BSDSockets.Streams;
with Network.Processes;

with ProgramArguments;
with SimRegion;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure simreg is

   Configuration : Config.Config_Type;

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Register;

   ProgramArguments.Debug;

   Network.Config.LoadConfiguration
     (Configuration => Configuration);

   Config.Debug
     (Item => Configuration);

   SimRegion.Initialize
     (Configuration => Configuration);

   loop
      exit when SimRegion.Process;
   end loop;

   SimRegion.Finalize;
exception
   when E:others =>
      Put("Exception Name : " & Exception_Name(E));
      New_Line;
      Put("Message : " & Exception_Message(E));
      New_Line;
      Put("Traceback      :");
      New_Line;
      Put(Symbolic_TraceBack(E));
      New_Line;
      loop
         null;
      end loop;
end simreg;
