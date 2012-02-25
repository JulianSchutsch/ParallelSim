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
--   7.Feb 2012 Julian Schutsch
--     - Original version

-- Reason for implementation
--   Central point where a configuration (see Config) can be interpreted
--   and implementations for abstract network services can be selected.
with Network.Streams;
with Network.Processes;
with Config; use Config;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Network.Config is

   ImplementationNotFound : Exception;
   InvalidConfiguration   : Exception;

   type StreamImplementation_Type is
      record
         ImplementationIdentifier : Unbounded_String;
         NewServer  : Network.Streams.ServerConstructor;
         FreeServer : Network.Streams.ServerDestructor;
         NewClient  : Network.Streams.ClientConstructor;
         FreeClient : Network.Streams.ClientDestructor;
      end record;

   type ProcessesImplementation_Type is
      record
         ImplementationIdentifier : Unbounded_String;
         StoreConfig : Network.Processes.StoreConfigAccess;
         LoadConfig  : Network.Processes.LoadConfigAccess;
         Spawn       : Network.Processes.SpawnAccess;
      end record;

   type Implementation_Type is
      record
         Stream    : StreamImplementation_Type;
         Processes : ProcessesImplementation_Type;
      end record;

   procedure RegisterStreamImplementation
     (StreamImplementation : StreamImplementation_Type);

   function FindImplementation
     (Configuration : Config_Type)
     return Implementation_Type;

end;
