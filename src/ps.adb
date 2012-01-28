pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Network.Packets;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;

procedure Ps is

   Socket : BSDSockets.SocketID;

begin

   BSDSockets.Initialize;

   Socket:=BSDSockets.Socket(AddressFamily => BSDSockets.AF_INET,
                             SocketType    => BSDSockets.SOCK_STREAM,
                             Protocol      => BSDSockets.IPPROTO_ANY);

   BSDSockets.Bind(Socket => Socket,
                   Port   => 1024);

   BSDSockets.Finalize;

end Ps;
