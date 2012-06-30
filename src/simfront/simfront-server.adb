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

with SimNodes; use SimNodes;
with Network.Streams;
with Network.Packets;
with Basics; use Basics;
with FrontProtocol;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SimFront.Users; use SimFront.Users;
with Authentication;

package body SimFront.Server is

   type ReceiveStatus_Enum is
     (ReceiveStatusVerifyProtocol,
      ReceiveStatusWaitForCommand,
      ReceiveStatusProcessCommand);

   type ServerCallBack_Type is new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);

   type ChannelCallBack_Type is new Network.Streams.ChannelCallBack_Type with
      record
         Channel : Network.Streams.Channel_ClassAccess;
         User    : User_Type;
      end record;
   type ChannelCallBack_Access is access all ChannelCallBack_Type;

   overriding
   procedure Receive
     (Item : in out ChannelCallBack_Type);

   overriding
   procedure Disconnect
     (Item : in out ChannelCallBack_Type);
   ---------------------------------------------------------------------------

   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess:=null;
   ServerCallBack       : aliased ServerCallBack_Type;
   ReceiveStatus        : ReceiveStatus_Enum;
   CurrentCommand       : FrontProtocol.ServerCmd_Type;

   type Command_Access is
     access procedure
       (Item : in out ChannelCallBack_Type);

   procedure CommandPublicKey
     (Item : in out ChannelCallBack_Type) is

      use type Authentication.PublicKey_ClassAccess;

   begin
      if Item.User.PublicKey/=null then
         Item.User.PublicKey.Free;
         Item.User.PublicKey:=null;
      end if;
   end CommandPublicKey;
   ---------------------------------------------------------------------------

   procedure CommandEncryptedMessage
     (Item : in out ChannelCallBack_Type) is
   begin
      null;
   end CommandEncryptedMessage;
   ---------------------------------------------------------------------------

   procedure CommandShutdown
     (Item : in out ChannelCallBack_Type) is
   begin
      null;
   end CommandShutdown;
   ---------------------------------------------------------------------------

   Commands : constant array (FrontProtocol.ServerCmd_Type range <>) of Command_Access:=
     (FrontProtocol.ServerCmdPublicKey        => CommandPublicKey'Access,
      FrontProtocol.ServerCmdEncryptedMessage => CommandEncryptedMessage'Access,
      FrontProtocol.ServerCmdShutdown         => CommandShutdown'Access);

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is

      pragma Unreferenced(Item);

      CallBack : ChannelCallBack_Access;

   begin

      CallBack         := new ChannelCallBack_Type;
      Channel.CallBack := Network.Streams.ChannelCallBack_ClassAccess(CallBack);
      CallBack.Channel := Channel;
      CallBack.User    := AnonymousUser;

   end AAccept;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : in out ChannelCallBack_Type) is
   begin

      Network.Streams.Free(Item.Channel.CallBack);

   end Disconnect;
   ---------------------------------------------------------------------------

   procedure Receive
     (Item : in out ChannelCallBack_Type) is

      PrevPosition : Integer:=0;

   begin

      loop

         PrevPosition:=Item.Channel.Position;
         case ReceiveStatus is
            when ReceiveStatusVerifyProtocol =>
               declare
                  ProtocolID : Unbounded_String;
               begin
                  ProtocolID:=Item.Channel.Read;
                  if ProtocolID/=FrontProtocol.ClientID then
                     -- TODO: Report error somewhere
                     Item.Channel.Disconnect;
                     return;
                  end if;
               end;
               ReceiveStatus:=ReceiveStatusWaitForCommand;

            when ReceiveStatusWaitForCommand =>
               CurrentCommand := Item.Channel.Read;
               if CurrentCommand not in Commands'Range then
                  Item.Channel.Disconnect;
                  -- TODO: Report Error somewhere
               end if;
               ReceiveStatus  := ReceiveStatusProcessCommand;

            when ReceiveStatusProcessCommand =>
               Commands(CurrentCommand).all(Item);

         end case;

      end loop;

   exception
      when Network.Packets.PacketOutOfData =>
         Item.Channel.Position:=PrevPosition;

   end Receive;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      StreamImplementation:=Network.Streams.Implementations.Find
        (Configuration => Configuration,
         Node          => U("Front"));
      ReceiveStatus:=ReceiveStatusVerifyProtocol;
      Server:=StreamImplementation.NewServer
        (Configuration => Configuration,
         Node          => U("Front"));
      Server.CallBack := ServerCallBack'Access;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeServer(Server);
   end Finalize;
   ---------------------------------------------------------------------------

end SimFront.Server;
