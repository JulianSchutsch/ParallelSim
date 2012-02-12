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

with SimCommon;

package body SimControl is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => SimControl,
      Name   => SimControlAccess);

   type StreamChannelStatus is
     (StreamChannelStatusWaitForID,
      StreamChannelStatusWaitForVersion);

   type StreamChannelCallBack is new Network.Streams.ChannelCallBack with
      record
         PSimControl : SimControlAccess;
         Status      : StreamChannelStatus:=StreamChannelStatusWaitForID;
      end record;

   type StreamChannelCallBackaccess is access StreamChannelCallBack;

   overriding
   procedure OnReceive
     (Item : in out StreamChannelCallBack) is
     use type SimCommon.ParallelSimNetworkIDString;

      NetworkID : SimCommon.ParallelSimNetworkIDString;

   begin
      loop
         case Item.Status is
            when StreamChannelStatusWaitForID =>
               begin
                  Put("Try Reading");
                  New_Line;
                  SimCommon.ParallelSimNetworkIDString'Read
                    (Item.Channel,
                     NetworkID);
                  if NetworkID=SimCommon.ParallelSimNetworkID then
                     Put("Valid Id...switch status");
                  else
                     Put("Oh...network ID incorrect, need to terminate the connection");
                  end if;
               exception
                  when Network.Streams.StreamOverflow =>
                     Put("Stream Overflow");
                     return;
               end;
            when others =>
               Put("Other Status..");
         end case;
      end loop;
   end OnReceive;
   ---------------------------------------------------------------------------

   type StreamServerCallBack is new Network.Streams.ServerCallBack with
      record
         PSimControl : SimControlAccess;
      end record;

   type StreamServerCallBackAccess is access StreamServerCallBack;

   overriding
   procedure OnAccept
     (Item : in out StreamServerCallBack;
      Chan : Network.Streams.ChannelClassAccess) is

      NewCallBack : StreamChannelCallBackAccess;

   begin

      NewCallBack             := new StreamChannelCallBack;
      NewCallBack.PSimControl := Item.PSimControl;
      NewCallBack.Channel     := Chan;

      Chan.CallBack
        :=Network.Streams.ChannelCallBackClassAccess(NewCallBack);

   end;
   ---------------------------------------------------------------------------

   function NewSimControl
     (NetworkImplementation : Network.Config.Implementation;
      Config                : CustomMaps.StringStringMap.Map)
      return SimControlAccess is

      Item : SimControlAccess;

      StreamServerConfig : CustomMaps.StringStringMap.Map;
      StreamServerCB     : StreamServerCallBackAccess;

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

      StreamServerCB:=new StreamServerCallBack;
      StreamServerCB.PSimControl:=Item;

      Item.StreamServer.CallBack
        :=Network.Streams.ServerCallBackClassAccess(StreamServerCB);
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
