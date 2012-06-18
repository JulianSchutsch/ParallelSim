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
with ByteOperations;

package MPI is

   type MPI_Comm_Type is new Interfaces.C.int;
   type MPI_Group_Type is new Interfaces.C.int;
   type Op_Type is new Interfaces.C.int;
   type MPI_Datatype_Type is new Interfaces.C.int;
   -- TODO: Extract the size of the request structure from the header files
   type MPI_Request_Type is new Interfaces.C.int;
   type MPI_Request_Access is access all MPI_Request_Type;

   type MPI_Status_Type is
      record
         count      : Interfaces.C.int;
         cancelled  : Interfaces.C.int;
         MPI_SOURCE : Interfaces.C.int;
         MPI_TAG    : Interfaces.C.int;
         MPI_ERROR  : Interfaces.C.int;
      end record;
   pragma Convention(C,MPI_Status_Type);
   type MPI_Status_Access is access all MPI_Status_Type;

   MPI_COMM_WORLD : constant:=MPIConstants.MPI_COMM_WORLD;
   -- Error codes
   MPI_SUCCESS       : constant:=MPIConstants.MPI_SUCCESS;
   MPI_ERR_BUFFER    : constant:=MPIConstants.MPI_ERR_BUFFER;
   MPI_ERR_COUNT     : constant:=MPIConstants.MPI_ERR_COUNT;
   MPI_ERR_TYPE      : constant:=MPIConstants.MPI_ERR_TYPE;
   MPI_ERR_TAG       : constant:=MPIConstants.MPI_ERR_TAG;
   MPI_ERR_COMM      : constant:=MPIConstants.MPI_ERR_COMM;
   MPI_ERR_RANK      : constant:=MPIConstants.MPI_ERR_RANK;
   MPI_ERR_REQUEST   : constant:=MPIConstants.MPI_ERR_REQUEST;
   MPI_ERR_ROOT      : constant:=MPIConstants.MPI_ERR_ROOT;
   MPI_ERR_GROUP     : constant:=MPIConstants.MPI_ERR_GROUP;
   MPI_ERR_OP        : constant:=MPIConstants.MPI_ERR_OP;
   MPI_ERR_TOPOLOGY  : constant:=MPIConstants.MPI_ERR_TOPOLOGY;
   MPI_ERR_DIMS      : constant:=MPIConstants.MPI_ERR_DIMS;
   MPI_ERR_ARG       : constant:=MPIConstants.MPI_ERR_ARG;
   MPI_ERR_UNKNOWN   : constant:=MPIConstants.MPI_ERR_UNKNOWN;
   MPI_ERR_TRUNCATE  : constant:=MPIConstants.MPI_ERR_TRUNCATE;
   MPI_ERR_OTHER     : constant:=MPIConstants.MPI_ERR_OTHER;
   MPI_ERR_INTERN    : constant:=MPIConstants.MPI_ERR_INTERN;
   MPI_ERR_IN_STATUS : constant:=MPIConstants.MPI_ERR_IN_STATUS;
   MPI_ERR_LASTCODE  : constant:=MPIConstants.MPI_ERR_LASTCODE;

   MPI_ANY_SOURCE : constant:=MPIConstants.MPI_ANY_SOURCE;
   MPI_ANY_TAG    : constant:=MPIConstants.MPI_ANY_TAG;
   MPI_BYTE       : constant:=MPIConstants.MPI_BYTE;

   procedure MPI_Init
     (Argc : access Interfaces.C.int;
      Args : access Interfaces.C.Strings.chars_ptr);
   pragma Import(C,MPI_Init,"MPI_Init");

   procedure MPI_Finalize;
   pragma Import(C,MPI_Finalize,"MPI_Finalize");

   function MPI_Comm_Size
     (comm : MPI_Comm_Type;
      size : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,MPI_Comm_Size,"MPI_Comm_size");

   function MPI_Comm_Rank
     (comm : MPI_Comm_Type;
      rank : access Interfaces.C.int)
      return Interfaces.C.int;
   pragma Import(C,MPI_Comm_Rank,"MPI_Comm_rank");

   function MPI_Isend
     (buf      : ByteOperations.Byte_Access;
      count    : Interfaces.C.int;
      datatype : MPI_Datatype_Type;
      dest     : Interfaces.C.int;
      tag      : Interfaces.C.int;
      comm     : MPI_Comm_Type;
      request  : MPI_Request_Access)
      return Interfaces.C.int;
   pragma Import(C,MPI_Isend,"MPI_Isend");

   function MPI_Irecv
     (buf      : ByteOperations.Byte_Access;
      count    : Interfaces.C.int;
      datatype : MPI_Datatype_Type;
      source   : Interfaces.C.unsigned;
      tag      : Interfaces.C.int;
      comm     : MPI_Comm_Type;
      request  : MPI_Request_Access)
      return Interfaces.C.int;
   pragma Import(C,MPI_Irecv,"MPI_Irecv");

   function MPI_Test
     (request : MPI_Request_Access;
      flag    : access Interfaces.C.int;
      status  : access MPI_Status_Type)
      return Interfaces.C.int;
   pragma Import(C,MPI_Test,"MPI_Test");

   function MPI_Barrier
     (comm : MPI_Comm_Type)
      return Interfaces.C.int;
   pragma Import(C,MPI_Barrier,"MPI_Barrier");

   function ErrorToString
     (ErrorCode : Interfaces.C.int)
      return String;

end MPI;
