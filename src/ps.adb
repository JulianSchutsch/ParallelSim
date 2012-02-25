pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;
with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Streams;
with Network.Barrier;
with Network.Config;
with BSDSockets.Streams;
with Network.Messages;
with Network.Processes;

with SimControl;
with SimElement;
with SimAdmin;
with Config;
with Processes;

with ProgramArguments;

procedure Ps is

   NetworkImplementation: constant Network.Config.Implementation:=
     (NewStreamServer  => BSDSockets.Streams.NewStreamServer'Access,
      NewStreamClient  => BSDSockets.Streams.NewStreamClient'Access,
      FreeStreamServer => BSDSockets.Streams.FreeStreamServer'Access,
      FreeStreamClient => BSDSockets.Streams.FreeStreamClient'Access,
      SpawnProcesses   => Network.Processes.Spawn'Access);

   Conf : Config.Modules;

begin
   Network.Processes.Spawn
     (Program => "simctr" & To_String(Processes.Suffix),
      Configuration => Conf,
      Amount => 2);

end Ps;
