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
with Ada.Exceptions;

package body BSDSockets.Streams is
   use type Network.Streams.ServerCallBackClassAccess;
   use type Network.Streams.ChannelCallBackClassAccess;
   use type Ada.Streams.Stream_Element_Offset;

   Servers        : access Server := null;
   Clients        : access Client := null;

   type ServerChannelAccess is access ServerChannel;

   -- Procedure called by NewStreamClient to
   --  loop of GetAddrInfo data.
   procedure Next
     (Item : not null access Client) is

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

         exception
            when E:others =>
               Put(Ada.Exceptions.Exception_Information(E));
               CloseSocket(Socket => Item.SelectEntry.Socket);
         end;

         Item.CurrAddrInfo
           := Item.CurrAddrInfo.ai_next;

      else
         FreeAddrInfo
           (AddrInfo => Item.CurrAddrInfo);
      end if;

   end;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : not null access ServerChannel) is

   begin

      BSDSockets.RemoveEntry
        (Entr => Item.SelectEntry'Access);

      BSDSockets.Shutdown
        (Socket => Item.SelectEntry.Socket,
         Method => BSDSockets.SD_BOTH);

      BSDSockets.CloseSocket
        (Socket => Item.SelectEntry.Socket);

      if Item.LastChannel/=null then
         Item.LastChannel.NextChannel:=Item.NextChannel;
      else
         Item.Server.FirstChannel:=Item.NextChannel;
      end if;

      if Item.NextChannel/=null then
         Item.NextChannel.LastChannel:=Item.LastChannel;
      end if;

   end Finalize;
   ---------------------------------------------------------------------------

   function NewStreamServer
     (Config : CustomMaps.StringStringMap.Map)
      return Network.Streams.ServerClassAccess is

      Item : ServerAccess;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Port      : PortID;
      Family    : AddressFamilyEnum;

   begin

      Item := new Server;

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
      return Network.Streams.ServerClassAccess(Item);

   end NewStreamServer;
   ---------------------------------------------------------------------------

   procedure FreeStreamServer
     (Item : in out Network.Streams.ServerClassAccess) is

      Serv : ServerAccess;

   begin

      Serv:=ServerAccess(Item);

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
     (Config : CustomMaps.StringStringMap.Map)
      return Network.Streams.ClientClassAccess is

      Item      : ClientAccess;
      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Family    : AddressFamilyEnum;


   begin

      Item:=new Client(Max=>1023);

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

      Next(Item);

      Item.NextClient := Clients;
      if Clients/=null then
         Clients.LastClient:=Item;
      end if;
      Clients:=Item;

      return Network.Streams.ClientClassAccess(Item);

   end newStreamClient;
   ---------------------------------------------------------------------------

   procedure FreeStreamClient
     (Item : in out Network.Streams.ClientClassAccess) is

      Clie : ClientAccess;

   begin

      Clie:=ClientAccess(Item);

      BSDSockets.RemoveEntry
        (Entr => Clie.SelectEntry'Access);

      BSDSockets.Shutdown
        (Socket => Clie.SelectEntry.Socket,
         Method => BSDSockets.SD_BOTH);

      BSDSockets.CloseSocket
        (Socket => Clie.SelectEntry.Socket);

      if Clie.LastClient/=null then
         Clie.LastClient.NextClient:=Clie.NextClient;
      else
         Clients:=Clie.NextClient;
      end if;

      if Clie.NextClient/=null then
         Clie.NextClient.LastClient:=Clie.LastClient;
      end if;

      Network.Streams.Free(Item);
   end FreeStreamClient;
   ---------------------------------------------------------------------------

   procedure AAccept
     (Item : not null access Server) is

      NewSock          : BSDSockets.SocketID;
      NewServerChannel : ServerChannelAccess;
      Host             : Unbounded_String;
      Port             : BSDSockets.PortID;

   begin

      NewSock := BSDSockets.AAccept
        (Socket => Item.SelectEntry.Socket,
         Host   => Host,
         Port   => Port);

      Put("NewSock");
      Put(BSDSockets.ToString(NewSock));
      New_Line;

      NewServerChannel                    := new ServerChannel(Max=>1023);
      NewServerChannel.SelectEntry.Socket := NewSock;
      NewServerChannel.Server             := ServerAccess(Item);
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
           (Chan => Network.Streams.ChannelClassAccess(NewServerChannel));
      end if;


   end AAccept;
   ---------------------------------------------------------------------------

   procedure Send
     (Item : access Client) is

      SendAmount : Ada.Streams.Stream_Element_Count;

   begin
      SendAmount:=BSDSockets.Send
        (Socket => Item.SelectEntry.Socket,
         Data   => Item.WrittenContent(0..Item.WritePosition-1),
         Flags  => BSDSockets.MSG_NONE);

      Item.WrittenContent(0..Item.WritePosition-SendAmount-1)
        :=Item.WrittenContent(SendAmount..Item.WritePosition-1);

      Item.WritePosition := Item.WritePosition - SendAmount;
   end Send;
   ---------------------------------------------------------------------------

   procedure Recv
     (Item : access ServerChannel) is

      RecvAmount : Ada.Streams.Stream_Element_Count;

   begin
      Item.ReceivedContent(0..Item.AmountReceived-Item.ReceivePosition-1)
        :=Item.ReceivedContent(Item.ReceivePosition..Item.AmountReceived-1);

      Item.ReceivePosition:=0;

      RecvAmount:=BSDSockets.Recv
        (Socket => Item.SelectEntry.Socket,
         Data   => Item.ReceivedContent(Item.AmountReceived..Item.ReceivedContent'Last),
         Flags  => BSDSockets.MSG_NONE);

      Item.AmountReceived:=Item.AmountReceived+RecvAmount;

      if Item.CallBack/=null then
         Item.CallBack.OnReceive;
      end if;

   end Recv;
   ---------------------------------------------------------------------------

   procedure Process is

      ServerItem        : access Server := Servers;
      ClientItem        : access Client := Clients;
      ServerChannelItem : access ServerChannel;

   begin

      while ClientItem/=null loop

         if ClientItem.SelectEntry.Readable then
            Put("Something to read...");
            New_Line;
         end if;

         if ClientItem.SelectEntry.Writeable then
            Send
              (Item => ClientItem);
         end if;

         ClientItem:=ClientItem.NextClient;

      end loop;

      while ServerItem/=null loop

         if ServerItem.SelectEntry.Readable then
            New_Line;

            AAccept
              (Item => ServerItem);

         end if;

         ServerChannelItem:=ServerItem.FirstChannel;

         while ServerChannelItem/=null loop

            if ServerChannelItem.SelectEntry.Readable then
               Recv
                 (Item => ServerChannelItem);
            end if;

            ServerChannelItem:=ServerChannelItem.NextChannel;

         end loop;


         ServerItem:=ServerItem.NextServer;

      end loop;

   end;
   ---------------------------------------------------------------------------

end BSDSockets.Streams;
