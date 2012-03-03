with Ada.Text_IO; use Ada.Text_IO;

with BSDSockets.Streams;
with Network.Processes.Local;

with Network.Config;
with Config;
with ProgramArguments;
with SimControl;
with Processes;

--with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;

procedure SimCtr is

   Configuration : Config.Config_Type;

begin
   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;

   ProgramArguments.Debug;

   Network.Config.LoadConfiguration
     (Configuration => Configuration);

   Config.Debug
     (Item => Configuration);

   SimControl.Initialize
     (Configuration =>  Configuration);

   loop
      exit when SimControl.Process;
   end loop;

   SimControl.Finalize;
exception
   when E:others =>
      Put("Exception Name : " & Exception_Name(E));
      New_Line;
      Put("Message : " & Exception_Message(E));
      New_Line;
--      Put("Traceback      :");
--      New_Line;
--      Put(Symbolic_TraceBack(E));
--      New_Line;
      loop
         null;
      end loop;
end SimCtr;
