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

with ProcessLoop;
with Config;
with Network.Streams;
with Ada.Calendar;
with Packets;
with Basics; use Basics;
with NodeInfo;
with Expressions;
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

package body BSDSockets.Streams is

   ReceiveBufferSize : constant := 4096;

   type BSDSocketChannel_Type is new Network.Streams.Channel_Type with
      record
         SelectEntry        : aliased BSDSockets.SelectEntry_Type;
         FirstSendPacket    : Packets.Packet_ClassAccess := null;
         -- The current send packet is the last sendpacket in the list
         SendPacket         : Packets.Packet_ClassAccess := null;
         SendPacketPos      : Integer := 0;
         ReceivePacket      : Packets.Packet_ClassAccess := null;
         ReceivePacketPos   : Integer := 0;
         -- Indicates wether someone is processing this element
         -- at the moment or not. You must not delete the element
         -- if active is true.
         Active             : Boolean:=False;
      end record;

   overriding
   function SendBufferEmpty
     (Item : access BSDSocketChannel_Type)
      return Boolean;

   overriding
   procedure SendPacket
     (Item   : access BSDSocketChannel_Type;
      Packet : Packets.Packet_ClassAccess);
   ---------------------------------------------------------------------------

   procedure SendPacket
     (Item : access BSDSocketChannel_Type;
      Packet : Packets.Packet_ClassAccess) is

      use type Packets.Packet_ClassAccess;

   begin

      Packet.Next:=Item.FirstSendPacket;
      if Item.FirstSendPacket/=null then
         Item.FirstSendPacket.Last:=Packet;
      else
         Item.SendPacket:=Packet;
      end if;
      Item.FirstSendPacket:=Packet;

   end SendPacket;
   ---------------------------------------------------------------------------

   type ServerChannel_Type;
   type ServerChannel_Access is access all ServerChannel_Type;
   type Server_Type;
   type Server_Access is access all Server_Type;

   type ServerChannel_Type is new BSDSocketChannel_Type with
      record
         NextChannel : ServerChannel_Access;
         LastChannel : ServerChannel_Access;
         Server      : Server_Access;
      end record;

   overriding
   procedure Disconnect
     (Item : access ServerChannel_Type);
   ---------------------------------------------------------------------------

   type Server_Type is new Network.Streams.Server_Type with
      record
         Family       : Unbounded_String;
         SelectEntry  : aliased BSDSockets.SelectEntry_Type;
         NextServer   : Server_Access:=null;
         LastServer   : Server_Access:=null;
         FirstChannel : ServerChannel_Access:=null;
      end record;
   ---------------------------------------------------------------------------

   type ClientModeEnum is
     (ClientModeConnecting,
      ClientModeConnected,
      ClientModeFailedConnect,
      ClientModeDisconnected);

   type Client_Type;
   type Client_Access is access all Client_Type;
   type Client_Type is new BSDSocketChannel_Type with
      record
         FirstAddrInfo : AddrInfoAccess:=null;
         CurrAddrInfo  : AddrInfoAccess:=null;
         ClientMode    : ClientModeEnum:=ClientModeConnecting;
         LastTime      : Ada.Calendar.Time;
         Port          : PortID;
         NextClient    : Client_Access:=null;
         LastClient    : Client_Access:=null;
      end record;

   overriding
   procedure Disconnect
     (Item : access Client_Type);
   ---------------------------------------------------------------------------

   use type Network.Streams.ServerCallBack_ClassAccess;
   use type Network.Streams.ChannelCallBack_ClassAccess;

   Servers        : Server_Access  := null;
   Clients        : Client_Access := null;

   function SendBufferEmpty
     (Item : access BSDSocketChannel_Type)
      return Boolean is

      use type Packets.Packet_ClassAccess;

   begin

      return (Item.SendPacket=null);

   end SendBufferEmpty;
   ---------------------------------------------------------------------------

   -- Procedure called by NewStreamClient to
   --  loop of GetAddrInfo data.
   procedure Next
     (Item : not null Client_Access) is

      RetryConnect : Boolean:=False;

   begin

      Put(Item.all'Address);
      Put_Line("Next...");

      if Item.CurrAddrInfo/=null then

         begin

            Item.SelectEntry.Socket:=Socket(Item.CurrAddrInfo);
            BSDSockets.SetNonBlocking(Item.SelectEntry.Socket);

            Put_Line("Connect");
            Connect
              (Socket   => Item.SelectEntry.Socket,
               AddrInfo => Item.CurrAddrInfo,
               Port     => Item.Port);

            BSDSockets.AddEntry
              (List => BSDSockets.DefaultSelectList'Access,
               Entr => Item.SelectEntry'Access);

            return;

         exception
            when E:FailedConnect =>
               Put_Line("EXC:Failed Connect..");
               Put_Line(Ada.Exceptions.Exception_Message(E));
               CloseSocket(Socket => Item.SelectEntry.Socket);
            when others =>
               CloseSocket(Socket => Item.SelectEntry.Socket);
               raise;
         end;

         Item.CurrAddrInfo
           := BSDSockets.AddrInfo_Next
             (AddrInfo => Item.CurrAddrInfo);

      else

         if Item.CallBack/=null then

            Item.CallBack.FailedConnect(RetryConnect);

         end if;

         if not RetryConnect then
            FreeAddrInfo
              (AddrInfo => Item.FirstAddrInfo);
            Item.ClientMode:=ClientModeFailedConnect;
         else
            Put_Line("Retry Connect...");
            -- TODO: Maybe a new lookup would be a better idea.
            Item.CurrAddrInfo := Item.FirstAddrInfo;
            Item.LastTime     := Ada.Calendar.Clock;
         end if;

      end if;

   end;
   ---------------------------------------------------------------------------

   -- Finalize is called when connection fails
   procedure Finalize
     (Item : ServerChannel_Access) is

      VarItem : ServerChannel_Access;

   begin
      Put("FinalizeChannel");
      Put(Item.all'Address);
      New_Line;

      begin

         BSDSockets.RemoveEntry
           (Entr => Item.SelectEntry'Access);

      exception
         when BSDSockets.EntryNotAddedToAnyList =>
            null;
      end;

      begin
         BSDSockets.CloseSocket
           (Socket => Item.SelectEntry.Socket);
      exception
         when BSDSockets.FailedCloseSocket =>
            null;
      end;

      if not Item.Active then

         if Item.LastChannel/=null then
            Item.LastChannel.NextChannel:=Item.NextChannel;
         else
            Item.Server.FirstChannel:=Item.NextChannel;
         end if;

         if Item.NextChannel/=null then
            Item.NextChannel.LastChannel:=Item.LastChannel;
         end if;

         if Item.CallBack/=null then
            Item.CallBack.Disconnect;
         end if;

         Item.Received.Free;
         Item.Received:=null;

         Network.Streams.Free(Network.Streams.Channel_ClassAccess(VarItem));

      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   -- Finalize is called when connection fails
   -- Client is removed from the Client list
   procedure Finalize
     (Item : Client_Access) is
   begin

      begin
         BSDSockets.CloseSocket
           (Socket => Item.SelectEntry.Socket);
       exception
          when FailedCloseSocket =>
            null;
      end;

      begin
         BSDSockets.RemoveEntry(Item.SelectEntry'Access);
      exception
         when EntryNotAddedToAnyList =>
            null;
      end;

      if Item.LastClient/=null then
         Item.LastClient.NextClient:=Item.NextClient;
      else
         Clients:=Item.NextClient;
      end if;

      if Item.NextClient/=null then
         Item.NextClient.LastClient:=Item.LastClient;
      end if;

      Item.LastClient:=null;
      Item.NextClient:=null;

      if Item.CallBack/=null then
         Item.CallBack.Disconnect;
      end if;

      Item.ClientMode:=ClientModeDisconnected;

   end;
   ---------------------------------------------------------------------------

   function NewStreamServer
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Network.Streams.Server_ClassAccess is

      Item : Server_Access;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Port      : PortID;
      Family    : AddressFamilyEnum;

   begin

      Item := new Server_Type;

      PortStr   := Configuration.Element(Node&".BindPort");
      FamilyStr := Configuration.Element(Node&".Family");
      Host      := Configuration.Element(Node&".BindIP");
      Port      := PortID'Value(To_String(PortStr));
      if FamilyStr="IPv4" then
         Family:=AF_INET;
      else
         if FamilyStr="IPv6" then
            Family:=AF_INET6;
         else
            raise Network.Streams.InvalidData;
         end if;
      end if;

      Item.Family:=FamilyStr;

      Item.SelectEntry.Socket := Socket
        (AddressFamily => Family,
         SocketType    => SOCK_STREAM,
         Protocol      => IPPROTO_ANY);

      Bind(Socket => Item.SelectEntry.Socket,
           Port   => Port,
           Family => Family,
           Host   => To_String(Host));

      Listen(Socket  => Item.SelectEntry.Socket,
             Backlog => 0);

      BSDSockets.SetNonBlocking(Item.SelectEntry.Socket);

      BSDSockets.AddEntry
        (List => BSDSockets.DefaultSelectList'Access,
         Entr => Item.SelectEntry'Access);

      Item.NextServer := Servers;
      if Servers/=null then
         Servers.LastServer:=Item;
      end if;
      Servers:=Item;

      return Network.Streams.Server_ClassAccess(Item);

   end NewStreamServer;
   ---------------------------------------------------------------------------

   procedure FreeStreamServer
     (Item : in out Network.Streams.Server_ClassAccess) is

      Serv        : Server_Access;
      Channel     : ServerChannel_Access;
      NextChannel : ServerChannel_Access;

   begin

      Put_Line("FreeStreamServer");
      Serv:=Server_Access(Item);

      if Serv.LastServer/=null then
         Serv.LastServer.NextServer:=Serv.NextServer;
      else
         Servers:=Serv.NextServer;
      end if;

      if Serv.NextServer/=null then
         Serv.NextServer.LastServer:=Serv.LastServer;
      end if;

      BSDSockets.RemoveEntry
        (Entr => Serv.SelectEntry'Access);

      BSDSockets.CloseSocket
        (Socket => Serv.SelectEntry.Socket);

      Channel:=Serv.FirstChannel;
      while Channel/=null loop
         NextChannel:=Channel.NextChannel;
         Finalize(Channel);
         Channel:=NextChannel;
      end loop;

      Network.Streams.Free(Item);

   end FreeStreamServer;
   ---------------------------------------------------------------------------

   function NewStreamClient
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Network.Streams.Client_ClassAccess is

      Item      : Client_Access:=null;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Family    : AddressFamilyEnum;

   begin

      Item:=new Client_Type;

      PortStr   := Configuration.Element(Node&".RemotePort");
      FamilyStr := Configuration.Element(Node&".Family");
      Host      := Configuration.Element(Node&".RemoteIP");

      PortStr := Expressions.Process
        (String    => PortStr,
         Variables => NodeInfo.Variables);

      Item.Port := PortID'Value(To_String(PortStr));
      if FamilyStr="IPv4" then
         Family:=AF_INET;
      else
         if FamilyStr="IPv6" then
            Family:=AF_INET6;
         else
            raise Network.Streams.InvalidData;
         end if;
      end if;

      Item.FirstAddrInfo:=GetAddrInfo
        (AddressFamily => Family,
         SocketType    => SOCK_STREAM,
         Protocol      => IPPROTO_ANY,
         Host          => To_String(Host));

      Item.CurrAddrInfo := Item.FirstAddrInfo;

      Item.NextClient := Clients;
      if Clients/=null then
         Clients.LastClient:=Item;
      end if;

      Clients:=Item;

      Item.LastTime:=Ada.Calendar.Clock;

      Item.Received:=new Packets.Packet_Type;
      Item.Received.Content:=new ByteOperations.ByteArray_Type(0..ReceiveBufferSize-1);

      -- TODO: Is there a case when we just have to give up?
      Next(Item);

      return Network.Streams.Client_ClassAccess(Item);

   end newStreamClient;
   ---------------------------------------------------------------------------

   procedure FreeStreamClient
     (Item : in out Network.Streams.Client_ClassAccess) is

      use type Network.Streams.Client_ClassAccess;
      use type Packets.Packet_ClassAccess;

      Client : Client_Access;

   begin

      Client:=Client_Access(Item);

      if Client.FirstAddrInfo/=null then
         BSDSockets.FreeAddrInfo(Client.FirstAddrInfo);
         Client.FirstAddrInfo:=null;
      end if;

      begin
         BSDSockets.Shutdown
           (Socket => Client.SelectEntry.Socket,
            Method => BSDSockets.SD_BOTH);
      exception
         when BSDSockets.FailedShutdown =>
            null;
      end;

      Finalize
        (Item => Client);

      if Client.Received/=null then
         Packets.Free(Client.Received);
         Client.Received:=null;
      end if;

      Put_Line("FreeClient");
      Network.Streams.Free(Item);
      Put_Line("FREE::");

   end FreeStreamClient;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : access ServerChannel_Type) is
   begin
      Put_Line("Disconnect Called  for "&SocketID'Image(Item.SelectEntry.Socket));
      Finalize(ServerChannel_Access(Item));
   end Disconnect;
   ---------------------------------------------------------------------------

   procedure Disconnect
     (Item : access Client_Type) is
   begin
      Put_Line("Finalize Call");
      Finalize(Client_Access(Item));
   end;
   ---------------------------------------------------------------------------

   AcceptFailed : Boolean:=False;
   procedure AAccept
     (Item : not null Server_Access) is

      NewSock          : BSDSockets.SocketID;
      NewServerChannel : ServerChannel_Access;
      Host             : Unbounded_String;
      Port             : BSDSockets.PortID;

   begin

      Put_Line("Try Accept "&SocketID'Image(Item.SelectEntry.Socket));
      begin
         BSDSockets.AAccept
           (Socket    => Item.SelectEntry.Socket,
            Host      => Host,
            Port      => Port,
            NewSocket => NewSock);
      exception
         when BSDSockets.FailedAccept =>
            if not AcceptFailed then
               Put_Line("****** AcceptFail:"&SocketID'Image(Item.SelectEntry.Socket));
               AcceptFailed:=True;
            end if;
            return;
      end;

      Put_Line("Channel Creation for Server"&SocketID'Image(Item.SelectEntry.Socket));

      NewServerChannel                    := new ServerChannel_Type;
      NewServerChannel.SelectEntry.Socket := NewSock;
      NewServerChannel.Server             := Item;
      NewServerChannel.NextChannel        := Item.FirstChannel;
      NewServerChannel.LastChannel        := null;

      BSDSockets.SetNonBlocking(NewSock);

      NewServerChannel.PeerAddress.Insert
        (Key      => U("Host"),
         New_Item => Host);
      NewServerChannel.PeerAddress.Insert
        (Key      => U("Port"),
         New_Item => Trim
           (Source => U(BSDSockets.PortID'Image(Port)),
            Side   => Ada.Strings.Left));
      NewServerChannel.PeerAddress.Insert
        (Key      => U("Family"),
         New_Item => Item.Family);

      if NewServerChannel.NextChannel/=null then
         NewServerChannel.NextChannel.LastChannel:=NewServerChannel;
      end if;

      Item.FirstChannel := NewServerChannel;
      NewServerChannel.Received:=new Packets.Packet_Type;
      NewServerChannel.Received.Content:=new ByteOperations.ByteArray_Type(0..ReceiveBufferSize);

      BSDSockets.AddEntry
        (List => BSDSockets.DefaultSelectList'Access,
         Entr => NewServerChannel.SelectEntry'Access);

      if Item.CallBack/=null then
         Item.CallBack.AAccept
           (Channel => Network.Streams.Channel_ClassAccess(NewServerChannel));
      end if;

   end AAccept;

   ---------------------------------------------------------------------------
   function Send
     (Item : access BSDSocketChannel_Type'Class)
      return Boolean is

      use type Packets.Packet_ClassAccess;

      SendAmount   : Integer;
      PacketToFree : Packets.Packet_ClassAccess;

   begin

      while Item.SendPacket/=null loop
         BSDSockets.Send
           (Socket => Item.SelectEntry.Socket,
            Data   => Item.SendPacket.Content
              (Item.SendPacketPos..Item.SendPacket.Position-1),
            Flags  => BSDSockets.MSG_NONE,
            Send   => SendAmount);

         Item.SendPacketPos:=Item.SendPacketPos+SendAmount;
         if Item.SendPacketPos>=Item.SendPacket.Position then
            PacketToFree    := Item.SendPacket;
            Item.SendPacket := PacketToFree.Last;
            Item.SendPacketPos:=0;
            Packets.Free(PacketToFree);

            if Item.SendPacket=null then
               Item.FirstSendPacket:=null;
            end if;

         end if;

      end loop;

      return True;

   exception

      when BSDSockets.FailedSend =>
         return False;

   end Send;
   ---------------------------------------------------------------------------

   function Recv
     (Item : access BSDSocketChannel_Type'Class)
      return Boolean is

      RecvAmount : Integer;

   begin

      -- Clear the interval 0..Item.Position to make space for new content
      Item.Received.Content(0..Item.Received.Amount-Item.Received.Position-1)
        :=Item.Received.Content(Item.Received.Position..Item.Received.Amount-1);

      Item.Received.Amount  := Item.Received.Amount-Item.Received.Position;
      Item.Received.Position := 0;

      BSDSockets.Recv
        (Socket => Item.SelectEntry.Socket,
         Data   => Item.Received.Content(Item.Received.Amount..Item.Received.Content'Last),
         Flags  => BSDSockets.MSG_NONE,
         Read   => RecvAmount);

      Item.Received.Amount := Item.Received.Amount+RecvAmount;

      if Item.Received.Amount/=0 then
         if Item.CallBack/=null then
            Item.CallBack.Receive;
         end if;
      end if;
      return True;

   exception

      when BSDSockets.FailedRecv =>
         return False;

   end Recv;
   ---------------------------------------------------------------------------

   procedure Process
     (Object : AnyObject_ClassAccess) is

      pragma Unreferenced(Object);

      use type Ada.Calendar.Time;

      ServerItem            : Server_Access := Servers;
      ClientItem            : Client_Access := Clients;
      NextClientItem        : Client_Access;
      ServerChannelItem     : ServerChannel_Access;
      NextServerChannelItem : ServerChannel_Access;
      OperationSuccess      : Boolean;

   begin

      while ClientItem/=null loop

         NextClientItem:=ClientItem.NextClient;

         ClientItem.Active:=True;

         if ClientItem.ClientMode/=ClientModeConnecting then

            OperationSuccess:=True;

            if ClientItem.SelectEntry.Readable then
               OperationSuccess:=Recv(ClientItem);
            end if;

            if ClientItem.SelectEntry.Writeable then
               OperationSuccess:=Send(ClientItem) and OperationSuccess;
            end if;

            if not OperationSuccess then
               Put_Line("Operations Failed"&SocketID'Image(ClientItem.SelectEntry.Socket));
               ClientItem.Active:=False;
               Finalize(ClientItem);
            end if;

         else

            if ClientItem.SelectEntry.Writeable then
               Put_Line("Connecting Success"&SocketID'Image(ClientItem.SelectEntry.Socket));
               if ClientItem.CallBack/=null then
                  ClientItem.CallBack.Connect;
               end if;
              FreeAddrInfo(ClientItem.FirstAddrInfo);
              ClientItem.FirstAddrInfo:=null;
              ClientItem.ClientMode:=ClientModeConnected;
            else
               -- TODO : Currently a timeout of 1 second is assumed
               --        This should become a configurable value
               if Ada.Calendar.Clock-ClientItem.LastTime>1.0 then
                  Put_Line("Timeout for "&SocketID'Image(ClientItem.SelectEntry.Socket));
                  begin
                     BSDSockets.CloseSocket(ClientItem.SelectEntry.Socket);
                  exception
                     when FailedCloseSocket =>
                        null;
                  end;
                  -- This can fail during a Fail+Retry connect
                  begin
                     BSDSockets.RemoveEntry(ClientItem.SelectEntry'Access);
                  exception
                     when EntryNotAddedToAnyList =>
                        null;
                  end;
                  Next(ClientItem);
               end if;

            end if;

         end if;

         ClientItem.Active:=False;

         ClientItem:=NextClientItem;

      end loop;
      ------------------------------------------------------------------------

      while ServerItem/=null loop

         if ServerItem.SelectEntry.Readable then

            AAccept
              (Item => ServerItem);

         end if;

         ServerChannelItem:=ServerItem.FirstChannel;

         while ServerChannelItem/=null loop

            NextServerChannelItem := ServerChannelItem.NextChannel;

            ServerChannelItem.Active:=True;

            OperationSuccess:=True;

            if ServerChannelItem.SelectEntry.Readable then
               OperationSuccess:=Recv
                 (Item => ServerChannelItem);
            end if;

            if ServerChannelItem.SelectEntry.Writeable then
               OperationSuccess
                 :=Send(Item => ServerChannelItem)
                 and OperationSuccess;
            end if;

            if not OperationSuccess then
               ServerChannelItem.Active:=False;
               Finalize(ServerChannelItem);
            end if;

            ServerChannelItem.Active:=False;

            ServerChannelItem:=NextServerChannelItem;

         end loop;

         ServerItem:=ServerItem.NextServer;

      end loop;

   end;
   ---------------------------------------------------------------------------

   InitializeCount : Natural:=0;

   procedure Initialize is
   begin
      if InitializeCount=0 then
         -- The order here is important since BSDSockets.Process should be
         -- called before ProcessLoop.Process
         ProcessLoop.Add(Process'Access,null);
         BSDSockets.Initialize;
      end if;
      InitializeCount:=InitializeCount+1;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      InitializeCount:=InitializeCount-1;
      if InitializeCount=0 then
         BSDSockets.Finalize;
         ProcessLoop.Remove(Process'Access,null);
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   Implementation : constant Network.Streams.Implementation_Type:=
     (Initialize => Initialize'Access,
      Finalize   => Finalize'Access,
      NewServer  => NewStreamServer'Access,
      FreeServer => FreeStreamServer'Access,
      NewClient  => NewStreamClient'Access,
      FreeClient => FreeStreamClient'Access);
   Identifier : constant Unbounded_String:=U("BSDSockets");

   procedure Register is
   begin

      Network.Streams.Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Network.Streams.Implementations.Unregister(Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end BSDSockets.Streams;
