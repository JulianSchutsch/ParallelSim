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
--   21.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Config;
with Basics; use Basics;

package Logging.Server is

   type LogEvent_Access is
     access procedure
       (Source  : StringStringMap.Map;
        Level   : Level_Enum;
        Module  : Unbounded_String;
        Channel : Unbounded_String;
        Message : Unbounded_String);

   OnLogEvent : Logevent_Access:=null;

   procedure Initialize
     (Configuration : Config.Config_Type);

   procedure Finalize;

end Logging.Server;