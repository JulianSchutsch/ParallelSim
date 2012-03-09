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

with Network.Streams;
with Endianess;
with Ada.Streams;
with Types;
with SimCommon;
with AdminProtocol;
with Logging;
with Basics; use Basics;

package body SimControl.AdminServer is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusSend,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand);

   type SendStatus_Enum is
     (SendStatusReceive,
      SendStatusIdentify,
      SendStatusReady);
   ---------------------------------------------------------------------------

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         ReceiveStatus : ReceiveStatus_Enum;
         SendStatus    : SendStatus_Enum;
         Channel       : Network.Streams.Channel_ClassAccess;
         LogChannel    : Logging.Channel_ClassAccess;
      end record;

   type ServerChannelCallBack_Access is access ServerChannelCallBack_Type;

   overriding
   procedure OnCanSend
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess;
   ServerCallBack       : aliased ServercallBack_Type;
   CurrentCommand       : Types.Integer32;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess;
   LogMainChannel       : Logging.Channel_ClassAccess;
   ---------------------------------------------------------------------------

   function Cmd_AdminServerMessage
     (Item : in ServerChannelCallBack_Type)
      return Boolean is

      use type AdminProtocol.ServerCmd_Msg_NativeType;

      Message       : String(1..128);
      NetMessageLen : AdminProtocol.ServerCmd_Msg_NetworkType;
      MessageLen    : AdminProtocol.ServerCmd_Msg_NativeType;

   begin

      Endianess.LittleEndianInteger32'Read
        (Item.Channel,
         NetMessageLen);

      MessageLen := Endianess.From(NetMessageLen);
      if (MessageLen<0)
        or (MessageLen>AdminProtocol.ServerCmd_Msg_MaxLength) then
         Item.LogChannel.Write
           (Level => Logging.LevelInvalid,
            Message => "Invalid Length for Message packet");
         -- TODO : Instruct this channel to be deleted
         return True;
      end if;
      Item.LogChannel.Write
        (Level => Logging.LevelDebug,
         Message => AdminProtocol.ServerCmd_NativeType'Image
           (MessageLen));

      String'Read
        (Item.Channel,
         Message(1..Integer(MessageLen)));

      Item.LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Message:" & Message(1..Integer(MessageLen)));

      return True;
   end Cmd_AdminServerMessage;
   ---------------------------------------------------------------------------

   function Cmd_Shutdown
     (Item : in ServerChannelCallBack_Type)
      return Boolean is
   begin
      Item.LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Received shutdown command");
      Terminated:=True;
      return True;
   end Cmd_Shutdown;
   ---------------------------------------------------------------------------

   type CmdArray is array (AdminProtocol.ServerCmd_NativeType range <>) of
     access function
       (Item : in ServerChannelCallBack_Type) return Boolean;

   Cmds:constant CmdArray:=
     (AdminProtocol.ServerCmdMessage  => Cmd_AdminServerMessage'Access,
      AdminProtocol.ServerCmdShutdown => Cmd_Shutdown'Access);

   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type) is

      use type SimCommon.NetworkIDString;

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition:=Item.Channel.ReceivePosition;
         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identity : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Item.Channel,
                     Identity);
                  if Identity/=SimCommon.NetworkAdminClientID then
                     Item.LogChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Wrong identification of the client for Network Admin");
                     -- TODO: Kill channel
                     return;
                  end if;
                  Item.ReceiveStatus := ReceiveStatusSend;
                  Item.SendStatus    := SendStatusIdentify;
                  return;
               end;
            when ReceiveStatusSend =>
               raise ServerStatusError;
            when ReceiveStatusWaitForCommand =>
               declare
                  Command : AdminProtocol.ServerCmd_NetworkType;
               begin
                  AdminProtocol.ServerCmd_NetworkType'Read
                    (Item.Channel,
                     Command);
                  CurrentCommand:=Endianess.From(Command);
                  Item.LogChannel.Write
                    (Level => Logging.LevelCommonEvent,
                     Message => "Received Command :"&
                     Types.Integer32'Image(CurrentCommand));
                  if CurrentCommand not in Cmds'Range then
                     Item.LogChannel.Write
                       (Level => Logging.LevelFailure,
                        Message => "Received Command "&
                        Types.Integer32'Image(CurrentCommand)&" not in valid range");
                     -- TODO: Kill channel
                     return;
                  end if;
                  Item.ReceiveStatus:=ReceiveStatusProcessCommand;
                  Item.LogChannel.Write
                    (Level   => Logging.LevelCommonEvent,
                     Message => "Command Receive Status");
               end;
            when ReceiveStatusProcessCommand =>
               Item.LogChannel.Write
                 (Level => Logging.LevelCommonEvent,
                  Message => "Process Command");
               if Cmds(CurrentCommand).all
                 (Item=> Item) then
                  Item.ReceiveStatus:=ReceiveStatusWaitForCommand;
               end if;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.ReceivePosition := PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnCanSend
     (Item : in out ServerChannelCallBack_Type) is

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition:=Item.Channel.WritePosition;
         case Item.SendStatus is
            when SendStatusReceive =>
               return;
            when SendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Item.Channel,
                  SimCommon.NetworkAdminServerID);
               Item.ReceiveStatus := ReceiveStatusWaitForCommand;
               Item.SendStatus    := SendStatusReady;
               return;
            when SendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Item.LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Disconnected client");
      Item.LogChannel.FreeChannel;
      Network.Streams.Free(Item.Channel.CallBack);
   end OnDisconnect;
   ---------------------------------------------------------------------------

   procedure OnAccept
     (Item : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
      pragma Warnings(Off,Item);

      NewCallBack : ServerChannelCallBack_Access;

   begin
      LogMainChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Accept connection for Admin server");
      NewCallBack := new ServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ReceiveStatusWaitForIdentification;
      NewCallBack.SendStatus    := SendStatusReceive;
      NewCallBack.Channel       := Channel;
      LogContext.NewChannel
        (ChannelName => ConcatElements
           (Item      => Channel.PeerAddress,
            Separator => To_Unbounded_String(";")),
         Channel     => NewCallBack.LogChannel);
      Channel.CallBack
        := Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);
   end OnAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      LogImplementation:=
        Logging.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName    => To_Unbounded_String("Control.Admin"));
      LogContext.NewChannel
        (ChannelName => To_Unbounded_String("Server"),
         Channel     => LogMainChannel);

      StreamImplementation:=
        Network.Streams.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Admin.Network"));

      StreamImplementation.Initialize.all;

      Server
        :=StreamImplementation.NewServer
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String
                  ("Admin.Server.Network")).all);

      Server.CallBack := ServerCallBack'Access;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeServer
        (Item => Server);
      StreamImplementation.Finalize.all;

      LogImplementation.FreeContext
        (Item => LogContext);
   end Finalize;
   ---------------------------------------------------------------------------

end SimControl.AdminServer;
