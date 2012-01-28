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

   procedure Initialize is
   begin
      null;
   end Initialize;


   procedure Finalize is
   begin
      null;
   end Finalize;

end BSDSockets.Thin;
