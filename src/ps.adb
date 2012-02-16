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

with SimControl;
with SimElement;
with SimAdmin;

with ProgramArguments;

procedure Ps is

   type ApplicationTypeEnum is
     (ApplicationTypeAdmin,
      ApplicationTypeControl,
      ApplicationTypeElement,
      ApplicationTypeEntrance,
      ApplicationTypeClient);

   NetworkImplementation: constant Network.Config.Implementation:=
     (NewStreamServer  => BSDSockets.Streams.NewStreamServer'Access,
      NewStreamClient  => BSDSockets.Streams.NewStreamClient'Access,
      FreeStreamServer => BSDSockets.Streams.FreeStreamServer'Access,
      FreeStreamClient => BSDSockets.Streams.FreeStreamClient'Access);

   Config : StringStringMap.Map;

   ApplicationType : ApplicationTypeEnum:=ApplicationTypeAdmin;

begin

   ProgramArguments.Debug;

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

   case ApplicationType is
      when ApplicationTypeAdmin =>
         SimAdmin.Initialize
           (NetworkImplementation => NetworkImplementation,
            Config                => Config);
      when ApplicationTypeControl =>
         SimControl.Initialize
           (NetworkImplementation => NetworkImplementation,
            Config                => Config);
      when ApplicationTypeElement =>
         SimElement.Initialize
           (NetworkImplementation => NetworkImplementation,
            Config                => Config);
      when ApplicationTypeEntrance => null;
      when ApplicationTypeClient => null;
   end case;

   while true loop
      BSDSockets.Process;
      BSDSockets.Streams.Process;

      case ApplicationType is
         when ApplicationTypeAdmin =>
            exit when SimAdmin.Process;
         when ApplicationTypeControl =>
            SimControl.Process;
         when ApplicationTypeElement =>
            SimElement.Process;
         when ApplicationTypeEntrance => null;
         when ApplicationTypeClient => null;
      end case;


   end loop;

   case ApplicationType is
      when ApplicationTypeAdmin =>
         SimAdmin.Finalize;
      when ApplicationTypeControl =>
         SimControl.Finalize;
      when ApplicationTypeElement =>
         SimElement.Finalize;
      when ApplicationTypeEntrance => null;
      when ApplicationTypeClient => null;
   end case;

   BSDSockets.Finalize;
end Ps;
