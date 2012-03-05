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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Implementation_Type is private;
   IdentifierKey : Unbounded_String;

package Config.Implementations is

   ImplementationNotFound : Exception;

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type);

   function Find
     (Configuration : Config_Type;
      ModuleName    : Unbounded_String)
      return Implementation_Type;

   function Find
     (ImplementationName : Unbounded_String)
      return Implementation_Type;

   -- TODO : Add Listing function (Debug)

end Config.Implementations;
