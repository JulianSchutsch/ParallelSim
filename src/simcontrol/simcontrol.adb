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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with Network.Streams;
with Network.Config;

with Ada.Streams;

with ProcessLoop;
with SimCommon;

with SimControl.AdminServer;

package body SimControl is

   type ControlReceiveStatus is
     (ControlReceiveStatusWaitForIdentification,
      ControlReceiveStatusReady,
      ControlReceiveStatusInvalid);

   type ControlSendStatus is
     (ControlSendStatusReceive,
      ControlSendStatusIdentify,
      ControlSendStatusReady);

   ControlNetworkImplementation : Network.Config.Implementation_Type;
   ControlServer                : Network.Streams.Server_ClassAccess;

   type ControlServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         Channel       : Network.Streams.Channel_ClassAccess;
         ReceiveStatus : ControlReceiveStatus;
         SendStatus    : ControlSendStatus;
      end record;

   type ControlServerChannelCallBack_Access is
     access all ControlServerChannelCallBack_Type;

   overriding
   procedure OnCanSend
     (Item : in out ControlServerChannelCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ControlServerChannelCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ControlServerChannelCallBack_Type);

   procedure OnCanSend
     (Item : in out ControlServerChannelCallBack_Type) is

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Item.Channel.WritePosition;
         case Item.SendStatus is
            when ControlSendStatusReceive =>
               return;
            when ControlSendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Item.Channel,
                  SimCommon.NetworkControlServerID);
               Item.SendStatus := ControlSendStatusReady;
               Put("Identify as ContentServer");
               New_Line;
            when ControlSendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnReceive
     (Item : in out ControlServerChannelCallBack_Type) is

      use type SimCommon.NetworkIDString;

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Item.Channel.ReceivePosition;
         case Item.ReceiveStatus is
            when ControlReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Item.Channel,
                     Identification);
                  if Identification/=SimCommon.NetworkControlClientID then
                     Put("Identification of incomming connection is incorrect");
                     New_Line;
                     Item.ReceiveStatus:=ControlReceiveStatusInvalid;
                  else
                     Put("Identification accepted");
                     Item.ReceiveStatus:=ControlReceiveStatusReady;
                     Item.SendStatus:=ControlSendStatusIdentify;
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
         Item.Channel.ReceivePosition:= PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ControlServerChannelCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("OnDisconnect");
      New_Line;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   type ControlServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure OnAccept
     (Item : in out ControlServerCallBack_Type;
      Chan : Network.Streams.Channel_ClassAccess) is
      pragma Warnings(Off,Item);

      NewCallBack : ControlserverChannelCallBack_Access;

   begin
      NewCallBack:=new ControlServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ControlReceiveStatusWaitForIdentification;
      NewCallBack.SendStatus    := ControlSendStatusReceive;
      NewCallBack.Channel       := Chan;
      Chan.CallBack:=Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);
   end OnAccept;
   ---------------------------------------------------------------------------

   ControlServerCallBack : aliased ControlServerCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin
      SimControl.AdminServer.Initialize
        (Configuration => Configuration);

      ControlNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control.Network"));

      ControlNetworkImplementation.Streams.Initialize.all;

      ControlServer
        :=ControlNetworkImplementation.Streams.NewServer
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String
                  ("Control.Server.Network")).all);

      ControlServer.CallBack:=ControlServerCallBack'Access;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlNetworkImplementation.Streams.FreeServer
        (Item => ControlServer);

      ControlNetworkImplementation.Streams.Finalize.all;

      SimControl.AdminServer.Finalize;

   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return boolean is
   begin
      ProcessLoop.Process;
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimControl;
