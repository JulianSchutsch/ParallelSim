-------------------------------------------------------------------------------
--   Copyright 2011 Julian Schutsch
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

-- Revision History
--   27.Jan 2012 Julian Schutsch
--     - Original version
pragma Ada_2012;

with BSDSockets.Thin;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BSDSockets is
   use type Interfaces.C.int;

   function AddressFamilyToInt is
     new Ada.Unchecked_Conversion(Source => AddressFamilyEnum,
                                  Target => Interfaces.C.int);
   function SocketTypeToInt is
     new Ada.Unchecked_Conversion(Source => SocketTypeEnum,
                                  Target => Interfaces.C.int);
   function ProtocolToInt is
     new Ada.Unchecked_Conversion(Source => ProtocolEnum,
                                  Target => Interfaces.C.int);
   function BindFamilyToInt is
     new Ada.Unchecked_Conversion(Source => BindFamilyEnum,
                                  Target => Interfaces.C.int);

   procedure Listen
     (Socket  : SocketID;
      Backlog : Integer:=0) is
      Result : Interfaces.C.int;
   begin
      Result:=BSDSockets.Thin.Listen(Socket => Interfaces.C.int(Socket),
                                     Backlog => Interfaces.C.int(Backlog));
      if Result/=0 then
         raise FailedListen;
      end if;
   end;
   ---------------------------------------------------------------------------

   procedure Bind
     (Socket : SocketID;
      Port   : PortID;
      Family : BindFamilyEnum;
      Host   : String := "") is

      Addr   : aliased BSDSockets.Thin.SockAddr_In6;
      Result : Interfaces.C.int;
      HostPtr : aliased Interfaces.C.Strings.chars_ptr;

   begin

      HostPtr:=Interfaces.C.Strings.New_String(Host);

      Put("BindFamily");
      Put(Integer(BindFamilyToInt(Family)));
      New_Line;
      Put("Host:");
      Put(Host);
      New_Line;
      Put("Port:");
      Put(Integer(Port));
      New_Line;

      Result:=BSDSockets.Thin.INET_PTON
        (AddressFamily => BindFamilyToInt(Family),
         AddrString    => HostPtr,
         Buffer        => Addr.sin6_addr'Access);

      if Result/=0 then
         Put("Failed because address translation failed");
         New_Line;
         raise FailedBind;
      end if;

      Interfaces.C.Strings.Free(Item=>HostPtr);

      Addr.sin6_family   := Interfaces.C.short(BindFamilyToInt(Family));
      Addr.sin6_port     := BSDSockets.Thin.HTONS
        (Interfaces.C.unsigned_short(Port));
      Addr.sin6_flowinfo := 0;
      Addr.sin6_scope_id := 0;

      Put("AddrSize:");
      Put(Integer(Addr'Size/8));

      Result:=BSDSockets.Thin.Bind(Socket  => Interfaces.C.int(Socket),
                                   Name    => Addr'Access,
                                   NameLen => Addr'Size/8);
      if Result/=0 then
         raise FailedBind;
      end if;
   end Bind;
   ---------------------------------------------------------------------------

   function Socket
     (AddressFamily : AddressFamilyEnum;
      SocketType    : SocketTypeEnum;
      Protocol      : ProtocolEnum) return SocketID is
      Value         : Interfaces.C.int;
   begin
      Value:=BSDSockets.Thin.Socket(AddressFamilyToInt(AddressFamily),
                                    SocketTypeToInt(SocketType),
                                    ProtocolToInt(Protocol));
      Put(Integer(Value));
      if Value=-1 then
         Put(Integer(BSDSockets.Thin.Error));
         raise FailedSocket;
      end if;
      return SocketID(Value);
   end Socket;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      BSDSockets.Thin.Initialize;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      BSDSockets.Thin.Finalize;
   end Finalize;
   ---------------------------------------------------------------------------

end BSDSockets;
