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

package body SimControl is

   StreamServer      : Network.Streams.ServerClassAccess;
   NetImplementation : Network.Config.Implementation_Type;
   NetConfig         : StringStringMap.Map;

   type StreamChannelStatus is
     (StreamChannelStatusWaitForID,
      StreamChannelStatusOther);

   Status     : StreamChannelStatus:=StreamChannelStatusWaitForID;

   type StreamChannelCallBack is new Network.Streams.ChannelCallBack with null record;
   type StreamChannelCallBackaccess is access StreamChannelCallBack;

   overriding
   procedure OnReceive
     (Item : in out StreamChannelCallBack) is
      use type SimCommon.NetworkIDString;
      use type Types.Integer32;

      NetworkID : SimCommon.NetworkIDString;
      Version   : Endianess.LittleEndianInteger32;

      Pos : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         Pos := Item.Channel.ReceivePosition;
         case Status is

            when StreamChannelStatusWaitForID =>

               SimCommon.NetworkIDString'Read
                 (Item.Channel,
                  NetworkID);

               Endianess.LittleEndianInteger32'Read
                 (Item.Channel,
                  Version);

               if (NetworkID=SimCommon.NetworkElementID) and
                 (Endianess.FromLittleEndian(Version)=10) then
                  Put("Correct ID and Version");
                  New_Line;
                  Status:=StreamChannelStatusOther;
               else
                  Put("Invalid connect header...");
                  New_Line;
                  -- TODO: Disconnect, report incident.
               end if;
            when others =>
               Put("Other Status..");
               New_Line;
               return; -- Dummy...
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.ReceivePosition:=Pos;
         Put("Stream Overflow");
         New_Line;
         return;
   end OnReceive;
   ---------------------------------------------------------------------------

   type StreamServerCallBack is new Network.Streams.ServerCallBack with null record;
   type StreamServerCallBackAccess is access StreamServerCallBack;

   overriding
   procedure OnAccept
     (Item : in out StreamServerCallBack;
      Chan : Network.Streams.ChannelClassAccess) is

      NewCallBack : StreamChannelCallBackAccess;

   begin

      NewCallBack            := new StreamChannelCallBack;
      NewCallBack.Channel    := Chan;

      Chan.CallBack
        :=Network.Streams.ChannelCallBackClassAccess(NewCallBack);

   end;
   ---------------------------------------------------------------------------

   procedure Initialize
     (NetworkImplementation : Network.Config.Implementation_Type;
      Config                : StringStringMap.Map) is

      StreamServerConfig : StringStringMap.Map;
      StreamServerCB     : StreamServerCallBackAccess;

   begin
      NetImplementation := NetworkImplementation;
      NetConfig         := Config;

      StreamServerConfig.Insert
        (To_Unbounded_String("Host"),
         Config.Element(To_Unbounded_String("ControlHost")));
      StreamServerConfig.Insert
        (To_Unbounded_String("Port"),
         Config.Element(To_Unbounded_String("ControlStreamPort")));
      StreamServerConfig.Insert
        (To_Unbounded_String("Family"),
         Config.Element(To_Unbounded_String("IPFamily")));

      StreamServer := NetworkImplementation.Stream.NewServer
        (Config => StreamServerConfig);

      StreamServerCB:=new StreamServerCallBack;

      StreamServer.CallBack
        :=Network.Streams.ServerCallBackClassAccess(StreamServerCB);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      NetImplementation.Stream.FreeServer
        (Item => StreamServer);
   end Finalize;
   ---------------------------------------------------------------------------

   procedure Process is
   begin
      null;
   end Process;
   ---------------------------------------------------------------------------

end SimControl;
