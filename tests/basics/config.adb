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

-- Test
--    Testing of Config package
--        * Adding of modules
--        * Saving and Loading

pragma Ada_2005;

with Config;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with UniqueStrings; use UniqueStrings;

procedure Test is

   Modules1 : Config.Modules;
   Modules2 : Config.Modules;


   procedure CreateRandomModules is
   begin
      for i in 0..99999 loop
         Put(UniqueRandomString);
         New_Line;
      end loop;
   end;

begin

   CreateRandomModules;

end Test;
