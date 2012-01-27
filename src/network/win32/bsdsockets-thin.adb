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

with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body BSDSockets.Thin is
   use type Interfaces.C.int;

   type WSAData is
      record
         wVersion       : Interfaces.Unsigned_16;
         wHighVersion   : Interfaces.Unsigned_16;
         szDescription  : Interfaces.C.char_array(0..256);
         szSystemStatus : Interfaces.C.char_array(0..128);
         iMaxSockets    : Interfaces.Integer_16;
         iMaxUdpDg      : Interfaces.Integer_16;
         lpVendorInfo   : Interfaces.C.Strings.chars_ptr;
      end record;
   pragma Convention(C,WSADATA);

   function WSAStartup(Version : Interfaces.C.unsigned_short;
                       Data    : access WSAData) return Interfaces.C.int;
   pragma Import(StdCall,WSAStartup,"WSAStartup");

   function WSACleanup return Interfaces.C.int;
   pragma Import(StdCall,WSACleanup,"WSACleanup");

   function WSAGetLastError return Interfaces.C.int;
   pragma Import(StdCall,WSAGetLastError,"WSAGetLastError");

   WSAInfo : aliased WSAData;

   function Error return Interfaces.C.int is
   begin
      return WSAGetLastError;
   end Error;

   procedure Initialize is
   begin
      if WSAStartup(16#0202#,WSAInfo'Access)/=0 then
         raise FailedNetworkAPIInitialization;
      end if;
   end Initialize;


   procedure Finalize is
   begin
      if WSACleanup/=0 then
         Put("Why is WSACleanup failing?");
         New_Line;
      end if;
      Put("BSDSockets.Thin.Finalize");
   end Finalize;

end BSDSockets.Thin;
