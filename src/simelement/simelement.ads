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
pragma Ada_2005;

with Network.Config;
with Network.Streams;
with CustomMaps;

package SimElement is

   type SimElement is tagged private;
   type SimElementAccess is access SimElement;

   function NewSimElement
     (NetworkImplementation : Network.Config.Implementation;
      Config                : CustomMaps.StringStringMap.Map)
      return SimElementAccess;

   procedure FreeSimElement
     (Item : in out SimElementAccess);

   procedure Process
     (Item : in out SimElement);

private

   type SimElement is tagged
      record
         StreamClient          : Network.Streams.ClientClassAccess;
         NetworkImplementation : Network.Config.Implementation;
         Config                : CustomMaps.StringStringMap.Map;
      end record;

end SimElement;
