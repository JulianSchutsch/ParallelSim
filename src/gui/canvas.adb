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

package body Canvas is

   procedure Clear
     (Canvas : in out BasicCanvas_Type;
      Color  : Color_Type) is
   begin
      for y in Canvas.Image'Range(1) loop
         for x in Canvas.Image'Range(2) loop
            Canvas.Image(y,x):=Color;
         end loop;
      end loop;
      Canvas.Modified := True;
   end;
   ---------------------------------------------------------------------------

end Canvas;
