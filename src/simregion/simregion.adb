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

with Network.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ProcessLoop;
with Ada.Streams;
with SimCommon;
with Logging;

package body SimRegion is

   type SendStatus_Enum is
     (SendStatusIdentify,
      SendStatusReceive,
      SendStatusReady);

   type ReceiveStatus_Enum is
     (ReceiveStatusSend,
      ReceiveStatusWaitForIdentification,
      ReceiveStatusReady,
      ReceiveStatusInvalid);

   StreamImplementation : Network.Streams.Implementation_Type;
   Client               : Network.Streams.Client_ClassAccess;
   SendStatus           : SendStatus_Enum;
   ReceiveStatus        : ReceiveStatus_Enum;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess;
   LogChannel           : Logging.Channel_ClassAccess;

   type ClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnConnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnCanSend
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ClientCallBack_Type);

   procedure OnReceive
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);

      use type SimCommon.NetworkIDString;

     PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      Put("On Receive");
      loop
         Put("*");
         Put(Integer(ReceiveStatus_Enum'Pos(ReceiveStatus)));
         PrevPosition := Client.ReceivePosition;
         case ReceiveStatus is
            when ReceiveStatusSend =>
               return;
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Client,
                     Identification);
                  if Identification/=SimCommon.NetworkControlServerID then
                     Put("Returned Identification of Control is invalid");
                     New_Line;
                     ReceiveStatus:=ReceiveStatusInvalid;
                  else
                     Put("Identification Control is valid");
                     New_Line;
                     ReceiveStatus:=ReceiveStatusReady;
                  end if;
               end;
               -- NOT YET VALID
               ReceiveStatus := ReceiveStatusReady;
               SendStatus    := SendStatusReady;
            when ReceiveStatusReady =>
               return;
            when ReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Client.ReceivePosition := PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnCanSend
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Client.WritePosition;
         case SendStatus is
            when SendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Client,
                  SimCommon.NetworkControlClientID);
               -- Check
               ReceiveStatus := ReceiveStatusWaitForIdentification;
               SendStatus    := SendStatusReceive;
            when SendStatusReceive =>
               return;
            when SendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Client.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelFailure,
         Message => "Failed to connect to Control network");
      Retry:=True;
   end;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Connected to Control network");
      SendStatus    := SendStatusIdentify;
      ReceiveStatus := ReceiveStatusSend;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Disconnected from Control network");
   end OnDisconnect;
   ---------------------------------------------------------------------------

   ClientCallBack : aliased ClientCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      LogImplementation
        :=Logging.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName    => To_Unbounded_String("Region.Control"));

      LogContext.NewChannel
        (ChannelName => To_Unbounded_String(""),
         Channel     => LogChannel);

      StreamImplementation
        :=Network.Streams.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control.Network"));

      StreamImplementation.Initialize.all;

      Client
        :=StreamImplementation.NewClient
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.Client.Network")).all);

      Client.CallBack:=ClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeClient
        (Item => Client);

      StreamImplementation.Finalize.all;
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
