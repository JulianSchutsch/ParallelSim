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
--   3.Feb 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   A barrier is a synchronisation method where each participating client
--   stops at a certain point to wait for all other clients to arrive at
--   the same point.
--   For very large numbers of clients, these barriers become notoriously
--   expensive in terms of network bandwidth costs.
--   This modul provides an abstract tagged type for easy replacement
--   of barrier implementations.

package Network.Barrier is
   type BarrierType is abstract tagged null record;

   procedure SignalReady
     (Barrier : in out BarrierType) is abstract;

   function AllReady
     (Barrier : in BarrierType)
      return Boolean is abstract;

end Network.Barrier;
