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
--   16.Feb 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--    Configuration for several modules using a single configuration unit
--    is easier if the configuration has a two level namespace structure.

-- Formats
--    Configuration file format used by SaveToFile and LoadFromFile
--        Text_IO based format writting the name of the module before saving
--        the content of the modules map. An empty line is used to indicate
--        no more modules are present.
--        The map is saved using two lines for each element, one for key and
--        one for value. An empty line is used to indicate there are no more
--        elemenets stored for this modul.

-- Usage
--    Modules manages a list of (Unbounded_String, StringStringMap)
--    Do not use empty names ("") as keys neither for module names nor
--    for map keys. Values can be empty names.
--    Special characters are not permitted at all.

--with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Config is

   InvalidName : Exception;

   type Config_Type is limited private;
   type Config_Access is access Config_Type;

--   procedure NewModule
--     (Item : in out Config_Type;
--      Name : Unbounded_String);

--   procedure Insert
--     (Item : in out Config_Type;
--      ModuleName : Unbounded_String;
--      Key        : Unbounded_String;
--      Value      : Unbounded_String);

--   function GetModuleMap
--     (Item : Config_Type;
--      Name : Unbounded_String)
--      return access StringStringMap.Map;

--   procedure SaveToFile
--     (Item     : in out Config_Type;
--      FileName : String);

--   procedure LoadFromFile
--     (Item     : in out Config_Type;
--      FileName : String);

--   procedure Clear
--     (Item : in out Config_Type);

--   procedure Debug
--     (Item : in out Config_Type);

   procedure Nothing;

private

   type Module;
   type ModuleAccess is access Module;

   type Module is
      record
--         Name : Unbounded_String;
--         Map  : aliased StringStringMap.Map;
         Last : ModuleAccess;
         Next : ModuleAccess;
      end record;

   type Config_Type is limited
      record
         First : ModuleAccess:=null;
      end record;

end Config;
