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
--   2.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Processes; use Processes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Network.Processes.Local is

   procedure Spawn
     (Program       : String;
      Amount        : Positive) is

   begin

      for i in 1..Amount loop

         Execute
           (ProgramName => Program,
            Arguments   => "-Network.Processes=Local");

      end loop;

   end Spawn;
   ---------------------------------------------------------------------------

   procedure StoreConfig
     (Configuration : in out Config_Type) is
   begin
      SaveToFile
        (Item     => Configuration,
         FileName => "_MyConfig");
   end StoreConfig;
   ---------------------------------------------------------------------------

   procedure LoadConfig
     (Configuration : in out Config_Type) is
   begin
      LoadFromFile
        (Item     => Configuration,
         FileName => "_MyConfig");
   end LoadConfig;
   ---------------------------------------------------------------------------

   procedure Initialize is null;
   procedure Finalize is null;

   Implementation : constant Network.Processes.Implementation_Type:=
     (Initialize               => Initialize'Access,
      Finalize                 => Finalize'Access,
      StoreConfig              => StoreConfig'Access,
      LoadConfig               => LoadConfig'Access,
      Spawn                    => Spawn'Access);

   procedure Register is
   begin
      Network.Processes.Implementations.Register
        (Identifier => To_Unbounded_String("Local"),
         Implementation => Implementation);
   end Register;

end Network.Processes.Local;
