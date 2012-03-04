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

with Ada.Text_IO; use Ada.Text_IO;
with Network.Config;
with ProcessLoop;

package body BSDSockets.Streams is
   use type Network.Streams.ServerCallBack_ClassAccess;
   use type Network.Streams.ChannelCallBack_ClassAccess;
   use type Ada.Streams.Stream_Element_Offset;

   Servers        : Server_Access  := null;
   Clients        : Client_Access := null;

   -- Procedure called by NewStreamClient to
   --  loop of GetAddrInfo data.
   procedure Next
     (Item : not null Client_Access) is

      RetryConnect : Boolean:=False;

   begin
      Put("Next...Connect");
      New_Line;
      if Item.CurrAddrInfo/=null then

         begin
            Put("SelectEntry.Assign(Socket)");
            New_Line;

            Item.SelectEntry.Socket:=Socket
              (AddrInfo => Item.CurrAddrInfo);

            Put("Calling Connect");
            New_Line;

            Connect
              (Socket   => Item.SelectEntry.Socket,
               AddrInfo => Item.CurrAddrInfo,
               Port     => Item.Port);

            Put("Add Entry for Client");

            BSDSockets.AddEntry
              (List => BSDSockets.DefaultSelectList'Access,
               Entr => Item.SelectEntry'Access);
            Put("Add Entry Done");

            Item.ClientMode:=ClientModeConnected;
            if Item.CallBack/=null then
               Put("Call Connect(ON)");
               Item.CallBack.OnConnect;
            end if;
            FreeAddrInfo
              (AddrInfo => Item.CurrAddrInfo);
            return;

         exception
            when others =>
               CloseSocket(Socket => Item.SelectEntry.Socket);
         end;

         Put("....Nexting");
         New_Line;

         Item.CurrAddrInfo
           := BSDSockets.AddrInfo_Next
             (AddrInfo => Item.CurrAddrInfo);

      else
         Put("Checking...");
         if Item.CallBack/=null then
            Item.CallBack.OnFailedConnect
              (Retry => RetryConnect);
         end if;

         if not RetryConnect then
            FreeAddrInfo
              (AddrInfo => Item.CurrAddrInfo);
         else
            Item.CurrAddrInfo:=Item.FirstAddrInfo;
            Item.LastTime:=Ada.Calendar.Clock;
         end if;

      end if;

   end;
   ---------------------------------------------------------------------------

   -- Finalize is called when connection fails
   procedure Finalize
     (Item : ServerChannel_Access) is

      VarItem : ServerChannel_Access;

   begin

      BSDSockets.RemoveEntry
        (Entr => Item.SelectEntry'Access);

      begin
         BSDSockets.CloseSocket
           (Socket => Item.SelectEntry.Socket);
      exception
         when BSDSockets.FailedCloseSocket =>
            null;
       end;

      if Item.LastChannel/=null then
         Item.LastChannel.NextChannel:=Item.NextChannel;
      else
         Item.Server.FirstChannel:=Item.NextChannel;
      end if;

      if Item.NextChannel/=null then
         Item.NextChannel.LastChannel:=Item.LastChannel;
      end if;

      if Item.CallBack/=null then
         Item.CallBack.OnDisconnect;
      end if;

      VarItem:=Item;

      Network.Streams.Free(Network.Streams.Channel_ClassAccess(VarItem));

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
         Item.CallBack.OnDisconnect;
      end if;

   end;
   ---------------------------------------------------------------------------

   function NewStreamServer
     (Config : StringStringMap.Map)
      return Network.Streams.Server_ClassAccess is

      Item : Server_Access;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Port      : PortID;
      Family    : AddressFamilyEnum;

   begin

      Item := new Server_Type;

      PortStr   := Config.Element(To_Unbounded_String("Port"));
      FamilyStr := Config.Element(To_Unbounded_String("Family"));
      Host      := Config.Element(To_Unbounded_String("Host"));
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

      BSDSockets.AddEntry
        (List => BSDSockets.DefaultSelectList'Access,
         Entr => Item.SelectEntry'Access);
      null;

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

      Serv : Server_Access;

   begin

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

      Network.Streams.Free(Item);
   end FreeStreamServer;
   ---------------------------------------------------------------------------

   function NewStreamClient
     (Config : StringStringMap.Map)
      return Network.Streams.Client_ClassAccess is

      Item      : Client_Access;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Family    : AddressFamilyEnum;


   begin

      Item:=new Client_Type(Max=>1023);

      PortStr   := Config.Element(To_Unbounded_String("Port"));
      FamilyStr := Config.Element(To_Unbounded_String("Family"));
      Host      := Config.Element(To_Unbounded_String("Host"));
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

      return Network.Streams.Client_ClassAccess(Item);

   end newStreamClient;
   ---------------------------------------------------------------------------

   procedure FreeStreamClient
     (Item : in out Network.Streams.Client_ClassAccess) is

      Clie : Client_Access;

   begin

      Clie:=Client_Access(Item);

      BSDSockets.Shutdown
        (Socket => Clie.SelectEntry.Socket,
         Method => BSDSockets.SD_BOTH);

      Finalize
        (Item => Clie);

      Network.Streams.Free(Item);
   end FreeStreamClient;
   ---------------------------------------------------------------------------

   procedure AAccept
     (Item : not null Server_Access) is

      NewSock          : BSDSockets.SocketID;
      NewServerChannel : ServerChannel_Access;
      Host             : Unbounded_String;
      Port             : BSDSockets.PortID;

   begin

      BSDSockets.AAccept
        (Socket    => Item.SelectEntry.Socket,
         Host      => Host,
         Port      => Port,
         NewSocket => NewSock);

      Put("NewSock");
      Put(BSDSockets.ToString(NewSock));
      New_Line;

      NewServerChannel                    := new ServerChannel_Type(Max=>1023);
      NewServerChannel.SelectEntry.Socket := NewSock;
      NewServerChannel.Server             := Item;
      NewServerChannel.NextChannel        := Item.FirstChannel;
      NewServerChannel.LastChannel        := null;

      if NewServerChannel.NextChannel/=null then
         NewServerChannel.NextChannel.LastChannel:=NewServerChannel;
      end if;

      Item.FirstChannel := NewServerChannel;

      BSDSockets.AddEntry
        (List => BSDSockets.DefaultSelectList'Access,
         Entr => NewServerChannel.SelectEntry'Access);

      if Item.CallBack/=null then
         Item.CallBack.OnAccept
           (Chan => Network.Streams.Channel_ClassAccess(NewServerChannel));
      end if;


   end AAccept;

   ---------------------------------------------------------------------------
   function Send
     (Item    : access BSDSocketChannel_Type'Class)
      return Boolean is

      SendAmount : Ada.Streams.Stream_Element_Count;

   begin
      if Item.WritePosition=0 then
         if Item.CallBack/=null then
            Item.CallBack.OnCanSend;
         end if;
         return True;
      end if;

      BSDSockets.Send
        (Socket => Item.SelectEntry.Socket,
         Data   => Item.WrittenContent(0..Item.WritePosition-1),
         Flags  => BSDSockets.MSG_NONE,
         Send   => SendAmount);

      Item.WrittenContent(0..Item.WritePosition-SendAmount-1)
        :=Item.WrittenContent(SendAmount..Item.WritePosition-1);

      Item.WritePosition := Item.WritePosition - SendAmount;

      return True;

   exception

      when BSDSockets.FailedSend =>
         return False;

   end Send;
   ---------------------------------------------------------------------------

   function Recv
     (Item : access BSDSocketChannel_Type'Class)
      return Boolean is

      RecvAmount : Ada.Streams.Stream_Element_Count;

   begin
      Item.ReceivedContent(0..Item.AmountReceived-Item.ReceivePosition-1)
        :=Item.ReceivedContent(Item.ReceivePosition..Item.AmountReceived-1);

      Item.AmountReceived  := Item.AmountReceived-Item.ReceivePosition;
      Item.ReceivePosition := 0;

      BSDSockets.Recv
        (Socket => Item.SelectEntry.Socket,
         Data   => Item.ReceivedContent(Item.AmountReceived..Item.ReceivedContent'Last),
         Flags  => BSDSockets.MSG_NONE,
         Read   => RecvAmount);

      Item.AmountReceived := Item.AmountReceived+RecvAmount;

      if Item.CallBack/=null then
         Item.CallBack.OnReceive;
      end if;
      return True;

   exception

      when BSDSockets.FailedRecv =>
         return False;

   end Recv;
   ---------------------------------------------------------------------------

   procedure Process is

      ServerItem            : Server_Access := Servers;
      ClientItem            : Client_Access := Clients;
      NextClientItem        : Client_Access;
      ServerChannelItem     : ServerChannel_Access;
      NextServerChannelItem : ServerChannel_Access;

      use type Ada.Calendar.Time;

      OperationSuccess : Boolean;

   begin

      while ClientItem/=null loop

         NextClientItem:=ClientItem.NextClient;

         OperationSuccess:=True;

         if ClientItem.SelectEntry.Readable then
            OperationSuccess:=Recv
              (Item => ClientItem);
         else
            if ClientItem.ClientMode=ClientModeConnecting then
               -- TODO : Currently a timeout of 1 second is assumed
               --        This should become a configurable value
               if Ada.Calendar.Clock-ClientItem.LastTime>1.0 then
                  Next
                    (Item=>ClientItem);
               end if;
            end if;
         end if;

         if ClientItem.SelectEntry.Writeable then
            OperationSuccess:=OperationSuccess and Send
              (Item => ClientItem);
         end if;

         if not OperationSuccess then
            Finalize
              (Item => ClientItem);
         end if;

         ClientItem:=NextClientItem;

      end loop;

      while ServerItem/=null loop

         if ServerItem.SelectEntry.Readable then

            AAccept
              (Item => ServerItem);

         end if;

         ServerChannelItem:=ServerItem.FirstChannel;

         while ServerChannelItem/=null loop

            NextServerChannelItem := ServerChannelItem.NextChannel;

            OperationSuccess:=True;

            if ServerChannelItem.SelectEntry.Readable then
               OperationSuccess:=Recv
                 (Item => ServerChannelItem);
            end if;

            if ServerChannelItem.SelectEntry.Writeable then
               OperationSuccess:=OperationSuccess and Send
                 (Item => ServerChannelItem);
            end if;

            if not OperationSuccess then
               Finalize(ServerChannelItem);
            end if;

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
         ProcessLoop.Add
           (Proc => Process'Access);
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
         ProcessLoop.Remove
           (Proc => Process'Access);
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

   StreamImplementation : constant Network.Config.StreamImplementation_Type:=
     (ImplementationIdentifier => To_Unbounded_String("BSDSockets.Stream"),
      Initialize => Initialize'Access,
      Finalize   => Finalize'Access,
      NewServer  => NewStreamServer'Access,
      FreeServer => FreeStreamServer'Access,
      NewClient  => NewStreamClient'Access,
      FreeClient => FreeStreamClient'Access);


   procedure Register is
   begin
      Network.Config.RegisterStreamImplementation
        (StreamImplementation => StreamImplementation);
   end Register;

end BSDSockets.Streams;
