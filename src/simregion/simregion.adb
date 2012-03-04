-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------
pragma Ada_2005;

with Network.Config;
with Network.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ProcessLoop;
with Ada.Streams;
with SimCommon;

package body SimRegion is

   type ControlSendStatus_Enum is
     (ControlSendStatusIdentify,
      ControlSendStatusReady);

   type controlReceiveStatus_Enum is
     (ControlReceiveStatusSend,
      ControlReceiveStatusWaitForIdentification,
      ControlReceiveStatusReady,
      ControlReceiveStatusInvalid);

   ControlNetworkImplementation : Network.Config.Implementation_Type;
   ControlClient                : Network.Streams.Client_ClassAccess;
   ControlSendStatus    : ControlSendStatus_Enum;
   ControlReceiveStatus : ControlReceiveStatus_Enum;

   type ControlClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure OnFailedConnect
     (Item  : in out ControlClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnConnect
     (Item : in out ControlClientCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ControlClientCallBack_Type);

   overriding
   procedure OnCanSend
     (Item : in out ControlClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ControlClientCallBack_Type);

   procedure OnReceive
     (Item : in out ControlClientCallBack_Type) is
      pragma Warnings(Off,Item);

      use type SimCommon.NetworkIDString;

     PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      Put("On Receive");
      loop
         Put("*");
         Put(Integer(ControlReceiveStatus_Enum'Pos(ControlReceiveStatus)));
         PrevPosition := ControlClient.ReceivePosition;
         case ControlReceiveStatus is
            when ControlReceiveStatusSend =>
               return;
            when ControlReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (ControlClient,
                     Identification);
                  if Identification/=SimCommon.NetworkControlServerID then
                     Put("Returned Identification of Control is invalid");
                     New_Line;
                     ControlReceiveStatus:=ControlReceiveStatusInvalid;
                  else
                     Put("Identification Control is valid");
                     New_Line;
                     ControlReceiveStatus:=ControlReceiveStatusReady;
                  end if;
               end;
            when ControlReceiveStatusReady =>
               return;
            when ControlReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         ControlClient.ReceivePosition := PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnCanSend
     (Item : in out ControlClientCallBack_Type) is
      pragma Warnings(Off,Item);

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := ControlClient.WritePosition;
         case ControlSendStatus is
            when ControlSendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (ControlClient,
                  SimCommon.NetworkControlClientID);
               ControlReceiveStatus:=ControlReceiveStatusWaitForIdentification;
            when ControlSendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         ControlClient.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ControlClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Warnings(Off,Item);
   begin
      Put("OnFailedConnect...");
      New_Line;
      Retry:=True;
   end;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ControlClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("Connected!");
      New_Line;
      ControlSendStatus    := ControlSendStatusIdentify;
      ControlReceiveStatus := ControlReceiveStatusSend;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ControlClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("Disconnected");
      New_Line;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   ControlClientCallBack : aliased ControlClientCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      ControlNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control.Network"));

      ControlNetworkImplementation.Streams.Initialize.all;

      ControlClient
        :=ControlNetworkImplementation.Streams.NewClient
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.Client.Network")).all);

      ControlClient.CallBack:=ControlClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlNetworkImplementation.Streams.FreeClient
        (Item => ControlClient);

      ControlNetworkImplementation.Streams.Finalize.all;
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return boolean is
   begin
      ProcessLoop.Process;
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimRegion;
