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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with SimCommon;
with Endianess;
with Types;

package body SimElement is

   StreamClient          : Network.Streams.ClientClassAccess;
   NetImplementation     : Network.Config.Implementation;
   NetConfig             : CustomMaps.StringStringMap.Map;

   procedure Initialize
     (NetworkImplementation : Network.Config.Implementation;
      Config                : CustomMaps.StringStringMap.Map) is

      StreamClientConfig : CustomMaps.StringStringMap.Map;

   begin

      NetImplementation := NetworkImplementation;
      NetConfig         := Config;

      StreamClientConfig.Insert
        (To_Unbounded_String("Host"),
         Config.Element(To_Unbounded_String("ControlHost")));
      StreamClientConfig.Insert
        (To_Unbounded_String("Port"),
         Config.Element(To_Unbounded_String("ControlStreamPort")));
      StreamClientConfig.Insert
        (To_Unbounded_String("Family"),
         Config.Element(To_Unbounded_String("IPFamily")));

      StreamClient := NetworkImplementation.NewStreamClient
        (Config => StreamClientConfig);

      -- Send Program ID
      SimCommon.NetworkIDString'Write
        (StreamClient,
         SimCommon.NetworkElementID);
      -- Send Version
      Endianess.LittleEndianInteger32'Write
        (StreamClient,
         Endianess.ToLittleEndian(10));

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      NetImplementation.FreeStreamClient
        (Item => StreamClient);
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Process is
   begin
      null;
   end Process;
   ---------------------------------------------------------------------------

end SimElement;
