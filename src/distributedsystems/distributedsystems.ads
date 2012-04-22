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

with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DistributedSystems is

   FailedNodeInitialization : Exception;

   type Group_Type is new Integer;
   type Node_Type is new Integer;

   MyGroup       : Group_Type;
   MyGlobalID    : Node_Type;
   FirstGlobalID : Node_Type;
   LastGlobalID  : Node_Type;
   MyGroupID     : Node_Type;
   FirstGroupID  : Node_Type;
   LastGroupID   : Node_Type;

   type InitializeNode_Access is
     access procedure
       (Configuration : out Config.Config_Type;
        Group         : Group_Type);

   type FinalizeNode_Access is
     access procedure;

   type Executable_Type is
      record
         Executable : Unbounded_String;
         Amount     : Positive;
      end record;

   type ExecutableArray_Type is array (Integer range <>) of Executable_Type;

   type SpawnNodes_Access is
     access procedure
       (Configuration : Config.Config_Type;
        Executables   : ExecutableArray_Type);

   type Implementation_Type is
      record
         InitializeNode : InitializeNode_Access := null;
         FinalizeNode   : FinalizeNode_Access   := null;
         SpawnNodes     : SpawnNodes_Access     := null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       =>
      To_Unbounded_String("DistributedSystemsImplementation"));

end DistributedSystems;
