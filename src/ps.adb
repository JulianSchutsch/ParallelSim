pragma Ada_2005;

with Network;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;
with Network.Processes.Local;
with ProgramArguments;

with Config;
with Processes;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;
with Logging.StdOut;

procedure Ps is

   Configuration           : Config.Config_Type;
   ProcessesImplementation : Network.Processes.Implementation_Type;

begin

   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;
   Logging.StdOut.Register;

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Network"),
      Key        => To_Unbounded_String("StreamImplementation"),
      Value      => To_Unbounded_String("BSDSockets.Stream"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Server.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Server.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("0.0.0.0"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Server.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10002"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Client.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Client.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("127.0.0.1"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Admin.Client.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10002"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Logging"),
      Key        => To_Unbounded_String("Implementation"),
      Value      => To_Unbounded_String("StdOut"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Network"),
      Key        => To_Unbounded_String("StreamImplementation"),
      Value      => To_Unbounded_String("BSDSockets.Stream"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Server.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Server.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10001"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Server.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("0.0.0.0"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Client.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Client.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10001"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.Client.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("127.0.0.1"));

   ProcessesImplementation:=Network.Processes.Implementations.Find
     (ImplementationName => To_Unbounded_String("Local"));
   -- The network for spawning isn't necessarily the same as the Control
   -- This should be a different module in the future.

   ProcessesImplementation.Initialize.all;

   Config.Debug
     (Item => Configuration);

   ProcessesImplementation.StoreConfig
     (Configuration => Configuration);

   ProcessesImplementation.Spawn
     (Program => "simctr" & To_String(Processes.Suffix),
      Amount  => 1);

   ProcessesImplementation.Spawn
     (Program => "simreg" & To_String(Processes.Suffix),
      Amount  => 1);

   ProcessesImplementation.Finalize.all;

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

end Ps;
