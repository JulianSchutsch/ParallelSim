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
with Network.Packets;
with Types;
with AdminProtocol;
with Logging;
with Basics; use Basics;

package body SimControl.AdminServer is

   type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand);

   ---------------------------------------------------------------------------

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         ReceiveStatus : ReceiveStatus_Enum;
         Channel       : Network.Streams.Channel_ClassAccess;
         LogChannel    : Logging.Channel_ClassAccess;
      end record;

   type ServerChannelCallBack_Access is access ServerChannelCallBack_Type;

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
   Server               : Network.Streams.Server_ClassAccess:=null;
   ServerCallBack       : aliased ServercallBack_Type;
   CurrentCommand       : Types.Integer32;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess:=null;
   LogMainChannel       : Logging.Channel_ClassAccess:=null;
   ---------------------------------------------------------------------------

   function Cmd_AdminServerMessage
     (Item : in ServerChannelCallBack_Type)
      return Boolean is

      use type AdminProtocol.ServerCmd_Msg_NativeType;

      Message       : Unbounded_String;

   begin

      Message:=Item.Channel.Read;
      Item.LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => To_String("Message:" & Message));

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

      PrevPosition : Integer;

      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition:=Item.Channel.Position;
         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identity : Unbounded_String;
               begin
                  Identity:=Item.Channel.Read;
                  if Identity/=AdminProtocol.ClientID then
                     Item.LogChannel.Write
                       (Level   => Logging.LevelInvalid,
                        Message => "Wrong identification of the client for Network Admin");
                     -- TODO: Kill channel
                  else
                     Item.LogChannel.Write
                       (Level => Logging.LevelEvent,
                        Message => "Admin Client has valid identification");
                  end if;
                  Item.ReceiveStatus := ReceiveStatusWaitForCommand;
               end;
            when ReceiveStatusWaitForCommand =>
               CurrentCommand:=Item.Channel.Read;
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
      when Network.Packets.PacketOutOfData =>
         Item.Channel.Position := PrevPosition;
   end OnReceive;
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
      Packet      : Network.Packets.Packet_Access;

   begin
      LogMainChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Accept connection for Admin server");
      NewCallBack := new ServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ReceiveStatusWaitForIdentification;
      NewCallBack.Channel       := Channel;
      LogContext.NewChannel
        (ChannelName => ConcatElements
           (Item      => Channel.PeerAddress,
            Separator => To_Unbounded_String(";")),
         Channel     => NewCallBack.LogChannel);
      Channel.CallBack
        := Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);

      Packet:=new Network.Packets.Packet_Type;
      Packet.Write(AdminProtocol.ServerID);
      Channel.SendPacket(Packet);
   end OnAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      LogImplementation:=
        Logging.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName    => To_Unbounded_String("Control.Admin"));
      LogContext.NewChannel
        (ChannelName => To_Unbounded_String("Server"),
         Channel     => LogMainChannel);

      StreamImplementation:=
        Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Network"));

      StreamImplementation.Initialize.all;

      Server
        :=StreamImplementation.NewServer
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Server.Network"));

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
