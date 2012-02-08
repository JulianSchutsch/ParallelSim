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

package body SimControl is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => SimControl,
      Name   => SimControlAccess);

   function NewSimControl
     (NetworkImplementation : Network.Config.Implementation;
      Config                : CustomMaps.StringStringMap.Map)
      return SimControlAccess is

      Item : SimControlAccess;

      StreamServerConfig : CustomMaps.StringStringMap.Map;

   begin
      Item:=new SimControl;
      Item.NetworkImplementation := NetworkImplementation;
      Item.Config                := Config;

      StreamServerConfig.Insert
        (To_Unbounded_String("Host"),
         Config.Element(To_Unbounded_String("ControlHost")));
      StreamServerConfig.Insert
        (To_Unbounded_String("Port"),
         Config.Element(To_Unbounded_String("ControlStreamPort")));
      StreamServerConfig.Insert
        (To_Unbounded_String("Family"),
         Config.Element(To_Unbounded_String("IPFamily")));

      Item.StreamServer := NetworkImplementation.NewStreamServer
        (Config => StreamServerConfig);

      return Item;

   end NewSimControl;
   ---------------------------------------------------------------------------

   procedure FreeSimControl
     (Item : in out SimControlAccess) is
   begin
      Free(Item);
   end FreeSimControl;
   ---------------------------------------------------------------------------

end SimControl;
