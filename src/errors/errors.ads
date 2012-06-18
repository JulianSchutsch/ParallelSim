--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

-- Revision History
--   18.Jun 2012 Julian Schutsch

-- Reasons for implementation
--   Create the means to handle or at least display errors which are not
--   expected.
--   A string representation for errors is introduced to attach error data
--   to exceptions.
pragma Ada_2005;

package Errors is

   type Error_Type is
      record
         null;
      end record;

   type Error_Access is access all Error_Type;

end Errors;
