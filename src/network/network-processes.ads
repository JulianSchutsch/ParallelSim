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
--  16.Feb 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   Spawn of an arbitrary number of processes independently of the currently
--   used system (MPI, local, ...)
--   This package provides an implementation for local (single computer)
--   spawn.

pragma Ada_2005;

with Config; use Config;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Network.Processes is

   FailedSpawn : Exception;

   type Spawn_Access is
     access procedure
       (Program : String;
        Amount  : Positive);

  type StoreConfig_Access is
     access procedure
       (Configuration : in out Config_Type);

  type LoadConfig_Access is
     access procedure
       (Configuration : in out Config_Type);
   -- This is a local implementation for Spawn, StoreConfig and LoadConfig
   -- meant to be used for programs run on a single computer

   type Initialize_Access is
     access procedure;

   type Finalize_Access is
     access procedure;
   ---------------------------------------------------------------------------

   type Implementation_Type is
      record
         Initialize  : Network.Processes.Initialize_Access:=null;
         Finalize    : Network.Processes.Finalize_Access:=null;
         StoreConfig : Network.Processes.StoreConfig_Access:=null;
         LoadConfig  : Network.Processes.LoadConfig_Access:=null;
         Spawn       : Network.Processes.Spawn_Access:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("ProcessesImplementation"));

end Network.Processes;
