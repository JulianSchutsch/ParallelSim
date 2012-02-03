pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Network.Packets;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;
with BSDSockets.Packets;
with CustomMaps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with System;
with System.Address_Image;

procedure Ps is
   use type BSDSockets.AddrInfoAccess;

--   ServerSocket : BSDSockets.SocketID;
--   ClientSocket : BSDSockets.SocketID;
--   AddrInfo     : BSDSockets.AddrInfoAccess;
--   pAddrInfo    : BSDSockets.AddrInfoAccess;
--   ClientHost   : String(1..1000);
--   ClientPort   : BSDSockets.PortID;
--   ClientConn   : BSDSOckets.SocketID;
   PacketServer       : BSDSockets.Packets.TCPServer;
   PacketServerConfig : CustomMaps.StringStringMap.Map;

begin

   BSDSockets.Initialize;

   PacketServerConfig.Insert(To_Unbounded_String("Host"),To_Unbounded_String("127.0.0.1"));
   PacketServerConfig.Insert(To_Unbounded_String("Port"),To_Unbounded_String("10001"));
   PacketServerConfig.Insert(To_Unbounded_String("Family"),To_Unbounded_String("IPv4"));

   PacketServer.Initialize(Config => PacketServerConfig);

   PacketServer.Finalize;

--   ServerSocket:=BSDSockets.Socket(AddressFamily => BSDSockets.AF_INET,
--                                   SocketType    => BSDSockets.SOCK_STREAM,
--                                   Protocol      => BSDSockets.IPPROTO_ANY);

--   BSDSockets.Bind(Socket => ServerSocket,
--                   Family => BSDSockets.AF_INET,
--                   Port   => 10001,
--                   Host   => "127.0.0.1");

--   BSDSockets.Listen(Socket  => ServerSocket,
--                     Backlog => 0);

--   AddrInfo:=BSDSockets.GetAddrInfo(AddressFamily => BSDSockets.AF_INET,
--                                    SocketType    => BSDSockets.SOCK_STREAM,
--                                    Protocol      => BSDSockets.IPPROTO_ANY,
--                                    Host          => "127.0.0.1");

--   New_Line;
--   if AddrInfo/=Null then
--      pAddrInfo:=AddrInfo;
--      AddrInfoLoop:
--      while pAddrInfo/=Null loop
--         ClientSocket:=BSDSockets.Socket(pAddrInfo);
--         BSDSockets.Connect(Socket   => ClientSocket,
--                            AddrInfo => pAddrInfo,
--                            Port     => 10001);
--         pAddrInfo:=BSDSockets.AddrInfo_Next(AddrInfo);
--         exit;
--      end loop AddrInfoLoop;
--
--      BSDSockets.FreeAddrInfo(AddrInfo);
--   end if;

--   BSDSockets.SSelect(Set);

--   while not Set(0).Readable loop
--      Put("*");
--   end loop;

--   ClientConn:=BSDSockets.AAccept(ServerSocket,ClientHost,ClientPort);
--   Put("Server Socket");
--   Put(BSDSockets.ToString(ServerSocket));
--   New_Line;
--   Put("Client Socket");
--   Put(BSDSockets.ToString(ClientSocket));
--   New_Line;
--   Put("Connection Socket");
--   Put(BSDSockets.ToString(ClientConn));
--   New_Line;

--   BSDSockets.Finalize;

end Ps;
