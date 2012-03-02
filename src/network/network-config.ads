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
--  25.Feb 2012 Julian Schutsch
--     - Automatic implementation selection using configuration data

-- Reason for implementation
--   ParallelSim requires a flexible configuration of network modules.
--   This package allows selecting of arbitrary implementations using
--   only configuration data (Config_Type).

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Network.Streams;
with Network.Processes;
with Config; use Config;

package Network.Config is

   ImplementationNotFound            : Exception;
   InvalidConfiguration              : Exception;
   ConfigurationTypeParameterMissing : Exception;

   type StreamImplementation_Type is
      record
         ImplementationIdentifier : Unbounded_String;
         Initialize : Network.Streams.InitializeAccess:=null;
         Finalize   : Network.Streams.FinalizeAccess:=null;
         NewServer  : Network.Streams.ServerConstructor:=null;
         FreeServer : Network.Streams.ServerDestructor:=null;
         NewClient  : Network.Streams.ClientConstructor:=null;
         FreeClient : Network.Streams.ClientDestructor:=null;
      end record;

   type ProcessesImplementation_Type is
      record
         ImplementationIdentifier : Unbounded_String;
         Initialize  : Network.Processes.InitializeAccess:=null;
         Finalize    : Network.Processes.FinalizeAccess:=null;
         StoreConfig : Network.Processes.StoreConfigAccess:=null;
         LoadConfig  : Network.Processes.LoadConfigAccess:=null;
         Spawn       : Network.Processes.SpawnAccess:=null;
      end record;

   type Implementation_Type is
      record
         Streams   : StreamImplementation_Type;
         Processes : ProcessesImplementation_Type;
      end record;

   procedure RegisterStreamImplementation
     (StreamImplementation : StreamImplementation_Type);

   procedure RegisterProcessImplementation
     (ProcessesImplementation : ProcessesImplementation_Type);

   function FindImplementation
     (Configuration : Config_Type;
      ModuleName    : Unbounded_String)
      return Implementation_Type;

   procedure LoadConfiguration
     (Configuration : in out Config_Type);

end;
