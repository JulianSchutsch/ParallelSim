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
--   24.Apr 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with ExceptionOutput;
with DistributedSystems.UseImplementations;
with Network.UseImplementations;
with Authentication.Dummy;
with Logging.Client;
with Logging.StdOut;
with SimFront;
with ProgramArguments;
with Ada.Text_IO; use Ada.Text_IO;

procedure Front is
begin

   Network.UseImplementations.Register;
   DistributedSystems.UseImplementations.Register;
   Logging.Client.Register;
   Logging.StdOut.Register;
   Authentication.Dummy.Register;

   ProgramArguments.Initialize;

   SimFront.Initialize;

   loop
      exit when SimFront.Process;
   end loop;

   SimFront.Finalize;

   Authentication.Dummy.Unregister;
   Logging.StdOut.Unregister;
   Logging.Client.Unregister;
   DistributedSystems.UseImplementations.Unregister;
   Network.UseImplementations.Unregister;

exception
   when E:others =>
      Put_Line("Front Exception");
      ExceptionOutput.Put(E);
end Front;
