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

package body SimElement is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => SimElement,
      Name   => SimElementAccess);

   function NewSimElement
     (NetworkImplementation : Network.Config.Implementation;
      Config                : CustomMaps.StringStringMap.Map)
      return SimElementAccess is

      Item               : SimElementAccess;
      StreamClientConfig : CustomMaps.StringStringMap.Map;

   begin
      Item := new SimElement;

      Item.NetworkImplementation := NetworkImplementation;
      Item.Config                := Config;

      StreamClientConfig.Insert
        (To_Unbounded_String("Host"),
         Config.Element(To_Unbounded_String("ControlHost")));
      StreamClientConfig.Insert
        (To_Unbounded_String("Port"),
         Config.Element(To_Unbounded_String("ControlStreamPort")));
      StreamClientConfig.Insert
        (To_Unbounded_String("Family"),
         Config.Element(To_Unbounded_String("IPFamily")));

      Item.StreamClient := NetworkImplementation.NewStreamClient
        (Config => StreamClientConfig);

      SimCommon.ParallelSimNetworkIDString'Write
        (Item.StreamClient,
         SimCommon.ParallelSimNetworkID);

      return Item;

   end NewSimElement;
   ---------------------------------------------------------------------------

   procedure FreeSimElement
     (Item : in out SimElementAccess) is
   begin
      Free(Item);
   end FreeSimElement;
   ---------------------------------------------------------------------------

   procedure Process
     (Item : in out SimElement) is
   begin
      null;
   end Process;

end SimElement;
