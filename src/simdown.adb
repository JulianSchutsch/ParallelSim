pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Processes;
with ProgramArguments; use ProgramArguments;
with BSDSockets.Streams;
with Network.Processes.Local;
With ExceptionOutput;

with SimAdmin;
with Config;

procedure SimDown is

   Configuration : Config.Config_Type;
   ProcessesImplementation : Network.Processes.Implementation_Type;

begin
   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;

   -- TEMP : Assuming Network.Processes=Local, not necessary for the
   --        future, but since this is only a temp help program ok.

   ProcessesImplementation
     :=Network.Processes.Implementations.Find
       (ImplementationName => To_Unbounded_String("Local"));

   ProcessesImplementation.LoadConfig
     (Configuration => Configuration);

   Config.Debug
     (Item => Configuration);

   SimAdmin.Initialize
     (Configuration => Configuration);

   SimAdmin.WaitForConnection;

   SimAdmin.Finalize;

exception
   when E:others =>
      ExceptionOutPut.Put(E);
end SimDown;
