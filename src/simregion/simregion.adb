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

   type ContentSendStatus_Enum is
     (ContentSendStatusIdentify,
      ContentSendStatusReady);

   type ContentReceiveStatus_Enum is
     (ContentReceiveStatusSend,
      ContentReceiveStatusWaitForIdentification,
      ContentReceiveStatusReady,
      ContentReceiveStatusInvalid);

   ControlRegionNetworkImplementation : Network.Config.Implementation_Type;
   ContentClient                      : Network.Streams.ClientClassAccess;
   ContentSendStatus    : ContentSendStatus_Enum;
   ContentReceiveStatus : ContentReceiveStatus_Enum;

   type ContentClientCallBack_Type is
     new Network.Streams.ChannelCallBack with null record;

   overriding
   procedure OnFailedConnect
     (Item  : in out ContentClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnConnect
     (Item : in out ContentClientCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ContentClientCallBack_Type);

   overriding
   procedure OnCanSend
     (Item : in out ContentClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ContentClientCallBack_Type);

   procedure OnReceive
     (Item : in out ContentClientCallBack_Type) is

      use type SimCommon.NetworkIDString;

     PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      Put("On Receive");
      loop
         Put("*");
         Put(Integer(ContentReceiveStatus_Enum'Pos(ContentReceiveStatus)));
         PrevPosition := ContentClient.ReceivePosition;
         case ContentReceiveStatus is
            when ContentReceiveStatusSend =>
               return;
            when ContentReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (ContentClient,
                     Identification);
                  if Identification/=SimCommon.NetworkContentServerID then
                     Put("Returned Identification of Content is invalid");
                     New_Line;
                     ContentReceiveStatus:=ContentReceiveStatusInvalid;
                  else
                     Put("Identification Content is valid");
                     New_Line;
                     ContentReceiveStatus:=ContentReceiveStatusReady;
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
         ContentClient.ReceivePosition := PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnCanSend
     (Item : in out ContentClientCallBack_Type) is

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := ContentClient.WritePosition;
         case ContentSendStatus is
            when ContentSendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (ContentClient,
                  SimCommon.NetworkContentClientID);
               ContentReceiveStatus:=ContentReceiveStatusWaitForIdentification;
            when ContentSendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         ContentClient.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ContentClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Warnings(Off,Item);
   begin
      Put("OnFailedConnect...");
      New_Line;
      Retry:=True;
   end;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ContentClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("Connected!");
      New_Line;
      ContentSendStatus    := ContentSendStatusIdentify;
      ContentReceiveStatus := ContentReceiveStatusSend;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ContentClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Put("Disconnected");
      New_Line;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   ContentClientCallBack : aliased ContentClientCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      ControlRegionNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control<->Region.Network"));

      ControlRegionNetworkImplementation.Streams.Initialize.all;

      ContentClient
        :=ControlRegionNetworkImplementation.Streams.NewClient
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.ContentClient.Network")).all);

      ContentClient.CallBack:=ContentClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlRegionNetworkImplementation.Streams.FreeClient
        (Item => ContentClient);

      ControlRegionNetworkImplementation.Streams.Finalize.all;
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
