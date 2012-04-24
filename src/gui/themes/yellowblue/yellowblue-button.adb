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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body YellowBlue.Button is

   type Button_Type is new GUI.Button.Button_Type with
      record
         null;
      end record;
   type Button_Access is access Button_Type;

   overriding
   procedure ReportCaptionChange
     (Item    : access Button_Type;
      Caption : Unbounded_String) is
   begin
      null;
   end ReportCaptionChange;
   ---------------------------------------------------------------------------

   function NewButton
     (Parent : Object_ClassAccess)
      return Button_ClassAccess is

      Button : Button_Access;

   begin
      Button:=new Button_Type;
      Object_Access(Button).Initialize(Parent);
      return Button_ClassAccess(Button);
   end NewButton;
   ---------------------------------------------------------------------------

end YellowBlue.Button;
