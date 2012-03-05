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
--   5.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package Logging.StdOut is

   type Channel_Type is new Logging.Channel_Type with private;

   overriding
   procedure Write
     (Item    : in out Channel_Type;
      Message : Unbounded_String);

   procedure Register;

private

   type Channel_Type is new Logging.Channel_Type with
      record
         ModuleName  : Unbounded_String;
         ChannelName : Unbounded_String;
      end record;

end Logging.StdOut;
