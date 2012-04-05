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

with GUI.Themes.YellowBlue.Window; use GUI.Themes.YellowBlue.Window;
with GUI.Themes.YellowBlue.VerticalScrollBar;
use GUI.Themes.YellowBlue.VerticalScrollBar;

package body GUI.Themes.YellowBlue is

   Implementation : constant GUI.Themes.Implementation_Type:=
     (NewWindow              => NewWindow'Access,
      NewVerticalScrollBar   => NewVerticalScrollBar'Access,
      VerticalScrollBarWidth => VerticalScrollBarWidth);

   Identifier : constant Unbounded_String:=To_Unbounded_String("YellowBlue");

   procedure Register is
   begin

      GUI.Themes.Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister is
   begin

      GUI.Themes.Implementations.UnRegister
        (Identifier => Identifier);

   end UnRegister;
   ---------------------------------------------------------------------------

end GUI.Themes.YellowBlue;
