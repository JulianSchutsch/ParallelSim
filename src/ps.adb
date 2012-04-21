pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with BSDSockets.Streams;
with DistributedSystems.MPI;
with ProgramArguments;

with Config;

with Logging.StdOut;

with ExceptionOutput;

with Ada.Text_IO; use Ada.Text_IO;

procedure Ps is

   Configuration           : Config.Config_Type;
   ProcessesImplementation : DistributedSystems.Implementation_Type;

   Family            : constant String:="IPv6";
   BindHost          : constant String:="::";
   RemoteHost        : constant String:="::1";
   AdminServerPort   : constant String:="10002";

begin
   ProgramArguments.Initialize;
   BSDSockets.Streams.Register;
   DistributedSystems.MPI.Register;
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

   ProcessesImplementation:=DistributedSystems.Implementations.Find
     (ImplementationName => To_Unbounded_String("MPI"));
   -- The network for spawning isn't necessarily the same as the Control
   -- This should be a different module in the future.

   Config.Debug
     (Item => Configuration);

   Put("SpawnNodes:");
   New_Line;
   ProcessesImplementation.SpawnNodes
     (Configuration => Configuration,
      Executables =>
        ((Executable => To_Unbounded_String("simctr"),
          Amount     => 1),
         (Executable => To_Unbounded_String("simreg"),
          Amount     => 1)));
   Put("Done");
   New_Line;

   DistributedSystems.MPI.Unregister;

exception
   when E:others =>
      ExceptionOutput.Put(E);

end Ps;
