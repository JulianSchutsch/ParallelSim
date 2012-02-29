pragma Ada_2005;

with Network;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Config;

-- These Packets are included to provide implementations implicitly
pragma Warnings(OFF);
with BSDSockets.Streams;
pragma Warnings(ON);

with Config;
with Processes;

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Ada.Exceptions; use Ada.Exceptions;

procedure Ps is

   Configuration         : Config.Config_Type;
   NetworkImplementation : Network.Config.Implementation_Type;

begin

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control<->Region.Network"),
      Key        => To_Unbounded_String("StreamImplementation"),
      Value      => To_Unbounded_String("BSDSockets.Stream"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control<->Region.Network"),
      Key        => To_Unbounded_String("ProcessesImplementation"),
      Value      => To_Unbounded_String("Local"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentServer.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentServer.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10001"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentServer.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("0.0.0.0"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentClient.Network"),
      Key        => To_Unbounded_String("Family"),
      Value      => To_Unbounded_String("IPv4"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentClient.Network"),
      Key        => To_Unbounded_String("Port"),
      Value      => To_Unbounded_String("10001"));

   Config.Insert
     (Item       => Configuration,
      ModuleName => To_Unbounded_String("Control.ContentClient.Network"),
      Key        => To_Unbounded_String("Host"),
      Value      => To_Unbounded_String("127.0.0.1"));

   NetworkImplementation:=Network.Config.FindImplementation
     (ModuleName    => To_Unbounded_String("Control<->Region.Network"),
      Configuration => Configuration);

   NetworkImplementation.Processes.Initialize.all;

   Config.Debug
     (Item => Configuration);

   NetworkImplementation.Processes.StoreConfig
     (Configuration => Configuration);

   NetworkImplementation.Processes.Spawn
     (Program => "simctr" & To_String(Processes.Suffix),
      Amount  => 1);

   NetworkImplementation.Processes.Spawn
     (Program => "simreg" & To_String(Processes.Suffix),
      Amount  => 1);

   NetworkImplementation.Processes.Finalize.all;

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
