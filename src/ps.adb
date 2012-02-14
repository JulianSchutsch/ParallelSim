pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;
with CustomMaps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Streams;
with Network.Barrier;
with Network.Config;
with BSDSockets.Streams;
with Network.Messages;

with SimControl;
with SimElement;

procedure Ps is

   NetworkImplementation: constant Network.Config.Implementation:=
     (NewStreamServer  => BSDSockets.Streams.NewStreamServer'Access,
      NewStreamClient  => BSDSockets.Streams.NewStreamClient'Access,
      FreeStreamServer => BSDSockets.Streams.FreeStreamServer'Access,
      FreeStreamClient => BSDSockets.Streams.FreeStreamClient'Access);

   SimCtr : SimControl.SimControlAccess;
   SimEle : SimElement.SimElementAccess;

   Config : CustomMaps.StringStringMap.Map;

begin
   BSDSockets.Initialize;

   Config.Insert
     (To_Unbounded_String("ControlHost"),
      To_Unbounded_String("127.0.0.1"));
   Config.Insert
     (To_Unbounded_String("ControlStreamPort"),
      To_Unbounded_String("10010"));
   Config.Insert
     (To_Unbounded_String("IPFamily"),
      To_Unbounded_String("IPv4"));

   SimCtr := SimControl.NewSimControl
     (NetworkImplementation => NetworkImplementation,
      Config                => Config);

   SimEle := SimElement.NewSimElement
     (NetworkImplementation => NetworkImplementation,
      Config                => Config);

   for i in 0..3 loop
      BSDSockets.Process;
      BSDSockets.Streams.Process;
   end loop;

   SimControl.FreeSimControl(SimCtr);
   SimElement.FreeSimElement(SimEle);

   BSDSockets.Finalize;
end Ps;
