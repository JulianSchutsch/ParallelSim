with Config;

with BSDSockets.Streams;

with ProgramArguments;
with SimRegion;
with DistributedSystems;
with DistributedSystems.MPI;

with ExceptionOutput;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Logging.StdOut;

with Ada.Text_IO; use Ada.Text_IO;

procedure simreg is

   Configuration : Config.Config_Type;
   DistributedSystemsImpl : DistributedSystems.Implementation_Type;

begin
   Put("This is SIMREG");
   New_Line;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Logging.StdOut.Register;
   DistributedSystems.MPI.Register;

   ProgramArguments.Debug;

   DistributedSystemsImpl
     :=DistributedSystems.Implementations.Find
       (Configuration => ProgramArguments.Configuration,
        Node          => To_Unbounded_String("Arguments"));

   DistributedSystemsImpl.InitializeNode
     (Configuration => Configuration,
      Group         => 0); -- TODO: Not yet valid GroupID

   Config.Debug
     (Item => Configuration);

   SimRegion.Initialize
     (Configuration => Configuration);

   loop
      exit when SimRegion.Process;
   end loop;

   SimRegion.Finalize;

   DistributedSystemsImpl.FinalizeNode.all;

   DistributedSystems.MPI.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);
      loop
         null;
      end loop;
end simreg;
