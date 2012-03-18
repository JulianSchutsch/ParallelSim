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

pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;

package body Config.Implementations is

   type ImplementationDesc_Type is
      record
         Identifier     : Unbounded_String;
         Implementation : Implementation_Type;
      end record;

   package List_Pack is
     new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ImplementationDesc_Type,
      "=" => "=");

   List : List_Pack.List;

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type) is

   begin

      List.Append
        (New_Item =>
           (Identifier     => Identifier,
            Implementation => Implementation));

   end Register;
   ---------------------------------------------------------------------------
   function Find
     (ImplementationName : Unbounded_String)
      return Implementation_Type is

      use type List_Pack.Cursor;

      Cursor             : List_Pack.Cursor;
      ImplementationDesc : ImplementationDesc_Type;

   begin
      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         ImplementationDesc:=List_Pack.Element(Cursor);
         if ImplementationDesc.Identifier=ImplementationName then
            return ImplementationDesc.Implementation;
         end if;
         Cursor :=List_Pack.Next(Cursor);
      end loop;

      raise ImplementationNotFound;
   end Find;
   ---------------------------------------------------------------------------

   function Find
     (Configuration : Config_Type;
      Node          : Unbounded_String)
      return Implementation_Type is

   begin

      return Find
        (ImplementationName => Configuration.Element
           (Key => Node&"."&IdentifierKey));
   end Find;
   ---------------------------------------------------------------------------

end Config.Implementations;
