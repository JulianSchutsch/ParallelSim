with Config;

with BSDSockets.Streams;
with Network.Processes.Local;

with ProgramArguments;
with SimRegion;
with Processes;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Logging.StdOut;

procedure simreg is

   Configuration : Config.Config_Type;
   ProcessesImplementation : Network.Processes.Implementation_Type;

begin
   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;
   Logging.StdOut.Register;

   ProgramArguments.Debug;

   ProcessesImplementation
     :=Network.Processes.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        ModuleName    => To_Unbounded_String("Arguments"));
   ProcessesImplementation.LoadConfig
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
