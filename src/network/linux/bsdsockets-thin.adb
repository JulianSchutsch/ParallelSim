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

   function Error return Interfaces.C.int is
   begin
      return 0; -- errors are of course still possible, but i am not sure how
                -- to implement them on linux yet... (this is just a quick debug help
                -- which must disappear
   end Error;
   ---------------------------------------------------------------------------

   procedure FD_SET
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set(Integer(Socket)/32):=Set(Integer(Socket)/32) or 2**Integer(Socket);
   end FD_SET;
   ---------------------------------------------------------------------------

   procedure FD_CLR
     (Socket : SocketID;
      Set    : access fd_set_struct) is
   begin
      Set(Integer(Socket)/32):=Set(Integer(Socket)/32) and not (2**Integer(Socket));
   end FD_CLR;
   ---------------------------------------------------------------------------

   function FD_ISSET
     (Socket : SocketID;
      Set    : access fd_set_struct)
      return Interfaces.C.int is
   begin
      return Interfaces.C.int(Set(Integer(Socket)/32) and (2**Integer(Socket)));
   end FD_ISSET;
   ---------------------------------------------------------------------------

   procedure FD_ZERO
     (Set : access fd_set_struct) is
   begin
      for i in Set'Range loop
         Set(i):=0;
      end loop;
   end FD_ZERO;
   ---------------------------------------------------------------------------

end BSDSockets.Thin;
