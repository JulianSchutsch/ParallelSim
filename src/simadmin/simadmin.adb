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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Calendar;
with Network.Streams;
with ProcessLoop;

package body SimAdmin is

   StreamImplementation : Network.Streams.Implementation_Type;
   Client : Network.Streams.Client_ClassAccess;

   procedure WaitForConnection is

      use type Ada.Calendar.Time;

      StartTime : Ada.Calendar.Time;

   begin
      -- Terminate loop after 3 seconds
      StartTime := Ada.Calendar.Clock;
      loop
         ProcessLoop.Process;
         exit when (Ada.Calendar.Clock-StartTime)>15.0;
      end loop;
   end WaitForConnection;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin

      StreamImplementation
        := Network.Streams.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Admin.Network"));

      StreamImplementation.Initialize.all;

      Client
        :=StreamImplementation.NewClient
          (Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Admin.Client.Network")).all);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeClient
        (Item => Client);
      StreamImplementation.Finalize.all;
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return Boolean is

   begin
      Put("X");
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimAdmin;
