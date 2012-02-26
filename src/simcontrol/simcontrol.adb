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
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams;

with Types;
with Endianess;
with SimCommon;

with ProcessLoop;

package body SimControl is

   ControlElementNetworkImplementation : Network.Config.Implementation_Type;
   ContentServer                       : Network.Streams.ServerClassAccess;

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin
      ControlElementNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control<->Element.Network"));

      ControlElementNetworkImplementation.Streams.Initialize.all;

      ContentServer
        :=ControlElementNetworkImplementation.Streams.NewServer
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.ContentServer.Network")).all);

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlElementNetworkImplementation.Streams.FreeServer
        (Item => ContentServer);

      ControlElementNetworkImplementation.Streams.Finalize.all;

   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return boolean is
   begin
      ProcessLoop.Process;
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimControl;
