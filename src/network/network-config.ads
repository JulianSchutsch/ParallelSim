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
         Initialize : Network.Streams.Initialize_Access:=null;
         Finalize   : Network.Streams.Finalize_Access:=null;
         NewServer  : Network.Streams.Server_Constructor:=null;
         FreeServer : Network.Streams.Server_Destructor:=null;
         NewClient  : Network.Streams.Client_Constructor:=null;
         FreeClient : Network.Streams.Client_Destructor:=null;
      end record;

   type ProcessesImplementation_Type is
      record
         ImplementationIdentifier : Unbounded_String;
         Initialize  : Network.Processes.Initialize_Access:=null;
         Finalize    : Network.Processes.Finalize_Access:=null;
         StoreConfig : Network.Processes.StoreConfig_Access:=null;
         LoadConfig  : Network.Processes.LoadConfig_Access:=null;
         Spawn       : Network.Processes.Spawn_Access:=null;
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
