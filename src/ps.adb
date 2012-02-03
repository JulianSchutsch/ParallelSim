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

with Network.Streams;
with Network.Barrier;
with Network.Peers;
with BSDSockets.Streams;

with System;
with System.Address_Image;

procedure Ps is
   use type BSDSockets.AddrInfoAccess;

begin
   null;
end Ps;
