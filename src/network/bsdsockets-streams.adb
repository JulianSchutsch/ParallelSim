with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body BSDSockets.Streams is

   procedure Flush
     (Stream : in out Client) is
   begin
      null;
   end;

   procedure Initialize
     (Stream : in out Client;
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

      Stream.SelectEntry.Socket := Socket
        (AddressFamily => Family,
         SocketType    => SOCK_STREAM,
         Protocol      => IPPROTO_ANY);

--      Bind(Socket => Server.SelectEntry.Socket,
--           Port   => Port,
--           Family => Family,
--           Host   => To_String(Host));

--      Listen(Socket  => Server.SelectEntry.Socket,
--             Backlog => 0);

      BSDSockets.AddEntry
        (List => BSDSockets.DefaultSelectList'Access,
         Entr => Stream.SelectEntry'Access);
      null;
   end Initialize;

   procedure Finalize
     (Stream : in out Client) is
   begin
      BSDSockets.RemoveEntry
        (Entr => Stream.SelectEntry'Access);

      BSDSockets.Shutdown
        (Socket => Stream.SelectEntry.Socket,
         Method => BSDSockets.SD_BOTH);

      BSDSockets.CloseSocket
        (Socket => Stream.SelectEntry.Socket);

   end Finalize;

end BSDSockets.Streams;
