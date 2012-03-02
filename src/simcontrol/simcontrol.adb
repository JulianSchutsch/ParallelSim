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

package body SimControl is

   type ContentReceiveStatus is
     (ContentReceiveStatusWaitForIdentification,
      ContentReceiveStatusReady,
      ContentReceiveStatusInvalid);

   type ContentSendStatus is
     (ContentSendStatusReceive,
      ContentSendStatusIdentify,
      ContentSendStatusReady);

   ControlElementNetworkImplementation : Network.Config.Implementation_Type;
   ContentServer                       : Network.Streams.ServerClassAccess;

   type ContentServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack with
      record
         Channel       : Network.Streams.ChannelClassAccess;
         ReceiveStatus : ContentReceiveStatus;
         SendStatus    : ContentSendStatus;
      end record;

   type ContentServerChannelCallBack_Access is
     access all ContentServerChannelCallBack_Type;

   overriding
   procedure OnCanSend
     (Item : in out ContentServerChannelCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ContentServerChannelCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ContentServerChannelCallBack_Type);

   procedure OnCanSend
     (Item : in out ContentServerChannelCallBack_Type) is

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Item.Channel.WritePosition;
         case Item.SendStatus is
            when ContentSendStatusReceive =>
               return;
            when ContentSendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Item.Channel,
                  SimCommon.NetworkContentServerID);
               Item.SendStatus := ContentSendStatusReady;
               Put("Identify as ContentServer");
               New_Line;
            when ContentSendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnReceive
     (Item : in out ContentServerChannelCallBack_Type) is

      use type SimCommon.NetworkIDString;

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Item.Channel.ReceivePosition;
         case Item.ReceiveStatus is
            when ContentReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Item.Channel,
                     Identification);
                  if Identification/=SimCommon.NetworkContentClientID then
                     Put("Identification of incomming connection is incorrect");
                     New_Line;
                     Item.ReceiveStatus:=ContentReceiveStatusInvalid;
                  else
                     Put("Identification accepted");
                     Item.ReceiveStatus:=ContentReceiveStatusReady;
                     Item.SendStatus:=ContentSendStatusIdentify;
                  end if;
               end;
            when ContentReceiveStatusReady =>
               return;
            when ContentReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.ReceivePosition:= PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ContentServerChannelCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("OnDisconnect");
      New_Line;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   type ContentServerCallBack_Type is
     new Network.Streams.ServerCallBack with null record;

   overriding
   procedure OnAccept
     (Item : in out ContentServerCallBack_Type;
      Chan : Network.Streams.ChannelClassAccess) is
      pragma Warnings(Off,Item);

      NewCallBack : ContentserverChannelCallBack_Access;

   begin
      NewCallBack:=new ContentServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ContentReceiveStatusWaitForIdentification;
      NewCallBack.SendStatus    := ContentSendStatusReceive;
      NewCallBack.Channel:=Chan;
      Chan.CallBack:=Network.Streams.ChannelCallBackClassAccess(NewCallBack);
   end OnAccept;
   ---------------------------------------------------------------------------

   ContentServerCallBack : aliased ContentServerCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin
      ControlElementNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control<->Region.Network"));

      ControlElementNetworkImplementation.Streams.Initialize.all;

      ContentServer
        :=ControlElementNetworkImplementation.Streams.NewServer
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.ContentServer.Network")).all);

      ContentServer.CallBack:=ContentServerCallBack'Access;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlElementNetworkImplementation.Streams.FreeServer
        (Item => ContentServer);

      ControlElementNetworkImplementation.Streams.Finalize.all;

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
