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

with Basics; use Basics;
with Config; use Config;

package Network.Processes is

   FailedSpawn : Exception;

   type SpawnAccess is
     access procedure
       (Program       : String;
        Configuration : Config.Modules;
        ProcessCount  : Positive);

   -- Assumes all processes are created on the current local node.
   procedure Spawn
     (Program       : String;
      Configuration : Config.Modules;
      Amount        : Positive);

end Network.Processes;
