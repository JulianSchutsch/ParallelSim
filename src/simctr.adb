with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;
with Network.Processes.Local;

with Config;
with ProgramArguments;
with SimControl;
with Processes;
with Logging.StdOut;
with ExceptionOutput;

procedure SimCtr is

   Configuration           : Config.Config_Type;
   ProcessesImplementation : Network.Processes.Implementation_Type;

begin
   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;
   Logging.StdOut.Register;

   ProcessesImplementation
     := Network.Processes.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        ModuleName    => To_Unbounded_String("Arguments"));

   ProcessesImplementation.LoadConfig
     (Configuration => Configuration);

   SimControl.Initialize
     (Configuration =>  Configuration);

   loop
      exit when SimControl.Process;
   end loop;

   SimControl.Finalize;
exception
   when E:others =>
      ExceptionOutput.Put(E);
      loop
         null;
      end loop;
end SimCtr;
