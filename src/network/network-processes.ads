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

with Config; use Config;

package Network.Processes is

   FailedSpawn : Exception;

   type SpawnAccess is
     access procedure
       (Program : String;
        Amount  : Positive);

  type StoreConfigAccess is
     access procedure
       (Configuration : in out Config_Type);

  type LoadConfigAccess is
     access procedure
       (Configuration : in out Config_Type);
   -- This is a local implementation for Spawn, StoreConfig and LoadConfig
   -- meant to be used for programs run on a single computer

   type InitializeAccess is
     access procedure;

   type FinalizeAccess is
     access procedure;

   procedure Spawn
     (Program : String;
      Amount  : Positive);

   procedure StoreConfig
     (Configuration : in out Config_Type);

   procedure LoadConfig
     (Configuration : in out Config_Type);

end Network.Processes;
