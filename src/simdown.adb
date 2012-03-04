pragma Ada_2005;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Processes;
with ProgramArguments; use ProgramArguments;
with BSDSockets.Streams;
with Network.Processes.Local;

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
   Config.Insert
     (Item       => ProgramArguments.Configuration,
      ModuleName => To_Unbounded_String("Arguments"),
      Key        => To_Unbounded_String("ProcessesImplementation"),
      Value      => To_Unbounded_String("Local"));

   ProcessesImplementation
     :=Network.Processes.Implementations.Find
       (Configuration => Configuration,
        ModuleName    => To_Unbounded_String("Arguments"));

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
      Put("Exception Name : " & Exception_Name(E));
      New_Line;
      Put("Message : " & Exception_Message(E));
      New_Line;
      Put("Traceback      :");
      New_Line;
      Put(Symbolic_TraceBack(E));
      New_Line;
end SimDown;
