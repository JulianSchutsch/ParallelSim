with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body BSDSockets.Streams is

   procedure Initialize
     (Item : not null access Server;
      Config : CustomMaps.StringStringMap.Map) is

      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Port      : PortID;
      Family    : AddressFamilyEnum;

   begin
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
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : not null access Server) is
   begin
      BSDSockets.RemoveEntry
        (Entr => Item.SelectEntry'Access);

      BSDSockets.CloseSocket
        (Socket => Item.SelectEntry.Socket);

   end Finalize;
   ---------------------------------------------------------------------------

   procedure Flush
     (Stream : in out Client) is
   begin
      null;
   end;
   ---------------------------------------------------------------------------

   procedure Next
     (Item : not null access Client) is

   begin
      if Item.CurrAddrInfo/=null then

         begin

            Item.SelectEntry.Socket:=Socket
              (AddrInfo => Item.CurrAddrInfo);

            Connect
              (Socket   => Item.SelectEntry.Socket,
               AddrInfo => Item.CurrAddrInfo,
               Port     => Item.Port);

            BSDSockets.AddEntry
              (List => BSDSockets.DefaultSelectList'Access,
               Entr => Item.SelectEntry'Access);

         exception
            when others =>
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


   procedure Initialize
     (Item   : not null access Client;
      Config : CustomMaps.StringStringMap.Map) is

      PortStr   : Unbounded_String;
      FamilyStr : Unbounded_String;
      Host      : Unbounded_String;
      Family    : AddressFamilyEnum;

   begin
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

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize
     (Item : not null access Client) is
   begin
      BSDSockets.RemoveEntry
        (Entr => Item.SelectEntry'Access);

      BSDSockets.Shutdown
        (Socket => Item.SelectEntry.Socket,
         Method => BSDSockets.SD_BOTH);

      BSDSockets.CloseSocket
        (Socket => Item.SelectEntry.Socket);

   end Finalize;
   ---------------------------------------------------------------------------

end BSDSockets.Streams;
