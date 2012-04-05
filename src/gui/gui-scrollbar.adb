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

package body GUI.ScrollBar is

   function GetMin
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.SPriv.Min;
   end GetMin;
   ---------------------------------------------------------------------------

   function GetMax
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.SPriv.Max;
   end GetMax;
   ---------------------------------------------------------------------------

   function GetPosition
     (Item : access ScrollBar_Type)
      return Integer is
   begin
      return Item.SPriv.Position;
   end GetPosition;
   ---------------------------------------------------------------------------

   procedure SetPosition
     (Item     : access ScrollBar_Type;
      Position : Integer) is

   begin

      if (Position<Item.SPriv.Min) or
        (Position>Item.SPriv.Max) then
         raise InvalidScrollBarPosition;
      end if;

      if (Item.SPriv.Position/=Position) then

         Item.SPriv.Position:=Position;
         ScrollBar_ClassAccess(Item).UpdateBarPosition;

         if Item.OnPositionChange/=null then
            Item.OnPositionChange(Item.CallBackObject);
         end if;

      end if;

   end SetPosition;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Item    : ScrollBar_Access;
      Parent  : Object_ClassAccess) is
   begin

      GUI.Initialize
        (Item   => Object_Access(Item),
         Parent => Parent);

   end Initialize;
   ---------------------------------------------------------------------------

   overriding
   procedure Finalize
     (Item : access ScrollBar_Type) is
   begin

      GUI.Finalize
        (Item => Object_Access(Item));

   end Finalize;
   ---------------------------------------------------------------------------

end GUI.ScrollBar;
