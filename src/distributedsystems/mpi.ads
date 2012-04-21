-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
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
--   20.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with MPIConstants;
with Interfaces.C;
with Interfaces.C.Strings;

package MPI is

   type Comm_Type is new Interfaces.C.int;
   type Group_Type is new Interfaces.C.int;
   type Op_Type is new Interfaces.C.int;

   COMM_WORLD : constant:=MPIConstants.MPI_COMM_WORLD;

   procedure Init
     (Argc : access Interfaces.C.int;
      Args : access Interfaces.C.Strings.chars_ptr);
   pragma Import(C,Init,"MPI_Init");

   procedure Finalize;
   pragma Import(C,Finalize,"MPI_Finalize");

   function Comm_Size
     (comm : Comm_Type;
      size : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Comm_Size,"MPI_Comm_Size");

   function Comm_Rank
     (comm : Comm_Type;
      rank : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,Comm_Rank,"MPI_Comm_Rank");

end MPI;
