with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;

with Config;
with ProgramArguments;
with SimControl;
with Logging.StdOut;
with ExceptionOutput;
with DistributedSystems;
with DistributedSystems.MPI;

with Ada.Text_IO; use Ada.Text_IO;

procedure SimCtr is

   Configuration           : Config.Config_Type;
   DistributedSystemsImpl  : DistributedSystems.Implementation_Type;

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   DistributedSystems.MPI.Register;
   Logging.StdOut.Register;

   DistributedSystemsImpl
     := DistributedSystems.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        Node          => To_Unbounded_String("Arguments"));

   Put("InitializeNode");
   New_Line;

   DistributedSystemsImpl.InitializeNode
     (Configuration => Configuration,
      Group         => 0); -- TODO:Group ID not yet valid

   Put("SIMCTR RECEIVED CONFIGURATION");
   New_Line;
   Config.Debug(Configuration);
   Put("DONE");
   New_Line;

   SimControl.Initialize
     (Configuration =>  Configuration);

   loop
      exit when SimControl.Process;
   end loop;

   DistributedSystemsImpl.FinalizeNode.all;
   SimControl.Finalize;
   DistributedSystems.MPI.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
      loop
         null;
      end loop;
end SimCtr;
