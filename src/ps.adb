pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;
with BSDSockets.Packets;
with CustomMaps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Streams;
with Network.Barrier;
with Network.Peers;
with BSDSockets.Streams;

procedure Ps is
   Server       : Network.Streams.ServerClassAccess;
   ServerConfig : CustomMaps.StringStringMap.Map;
   Client       : Network.Streams.ChannelClassAccess;
   ClientConfig : CustomMaps.StringStringMap.Map;

begin
   BSDSockets.Initialize;

   ServerConfig.Insert(To_Unbounded_String("Host"),To_Unbounded_String("127.0.0.1"));
   ServerConfig.Insert(To_Unbounded_String("Port"),To_Unbounded_String("10001"));
   ServerConfig.Insert(To_Unbounded_String("Family"),To_Unbounded_String("IPv4"));

   Server:=new BSDSockets.Streams.Server;
   Server.Initialize(Config => ServerConfig);

   ClientConfig.Insert(To_Unbounded_String("Host"),To_Unbounded_String("127.0.0.1"));
   ClientConfig.Insert(To_Unbounded_String("Port"),To_Unbounded_String("10001"));
   ClientConfig.Insert(To_Unbounded_String("Family"),To_Unbounded_String("IPv4"));

   Client:=new BSDSockets.Streams.Client(Max=>1023);
   Client.Initialize(Config => ClientConfig);

   BSDSockets.Process;


   Client.Finalize;

   Server.Finalize;

   Network.Streams.Free(Client);
   Network.Streams.Free(Server);

   BSDSockets.Finalize;
end Ps;
