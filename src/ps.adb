pragma Ada_2005;

with Network;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;
with Network.Processes.Local;
with ProgramArguments;

with Config;
with Processes;

with Logging.StdOut;

with ExceptionOutput;

procedure Ps is

   Configuration           : Config.Config_Type;
   ProcessesImplementation : Network.Processes.Implementation_Type;

   Family            : constant String:="IPv6";
   BindHost          : constant String:="::";
   RemoteHost        : constant String:="::1";
   AdminServerPort   : constant String:="10002";
   ControlServerPort : constant String:="10001";

begin
   Processes.Initialize;
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   Network.Processes.Local.Register;
   Logging.StdOut.Register;

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Network.StreamImplementation"),
      New_Item  => To_Unbounded_String("BSDSockets.Stream"));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Server.Network.Family"),
      New_Item  => To_Unbounded_String(Family));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Server.Network.Host"),
      New_Item  => To_Unbounded_String(BindHost));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Server.Network.Port"),
      New_Item  => To_Unbounded_String(AdminServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Client.Network.Family"),
      New_Item  => To_Unbounded_String(Family));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Client.Network.Host"),
      New_Item  => To_Unbounded_String(RemoteHost));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Admin.Client.Network.Port"),
      New_Item  => To_Unbounded_String(AdminServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Logging.Implementation"),
      New_Item  => To_Unbounded_String("StdOut"));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Network.StreamImplementation"),
      New_Item  => To_Unbounded_String("BSDSockets.Stream"));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Server.Network.Family"),
      New_Item  => To_Unbounded_String(Family));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Server.Network.Port"),
      New_Item  => To_Unbounded_String(ControlServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Server.Network.Host"),
      New_Item  => To_Unbounded_String(BindHost));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Client.Network.Family"),
      New_Item  => To_Unbounded_String(Family));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Client.Network.Port"),
      New_Item  => To_Unbounded_String(ControlServerPort));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("Control.Client.Network.Host"),
      New_Item  => To_Unbounded_String(RemoteHost));

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

--   ProcessesImplementation.Spawn
--     (Program => "simreg" & To_String(Processes.Suffix),
--      Amount  => 1);

   ProcessesImplementation.Finalize.all;

exception
   when E:others =>
      ExceptionOutput.Put(E);

end Ps;
