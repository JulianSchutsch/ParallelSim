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
with ProcessLoop;

package body SimRegion is

   ControlRegionNetworkImplementation : Network.Config.Implementation_Type;
   ContentClient                      : Network.Streams.ClientClassAccess;

   type ContentClientCallBack_Type is
     new Network.Streams.ChannelCallBack with null record;

   overriding
   procedure OnFailedConnect
     (Item  : in out ContentClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnConnect
     (Item : in out ContentClientCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ContentClientCallBack_Type);

   procedure OnFailedConnect
     (Item  : in out ContentClientCallBack_Type;
      Retry : in out Boolean) is
   begin
      Put("OnFailedConnect...");
      New_Line;
      Retry:=True;
   end;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ContentClientCallBack_Type) is
   begin
      Put("Connected!");
      New_Line;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ContentClientCallBack_Type) is
   begin
      Put("Disconnected");
      New_Line;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   ContentClientCallBack : aliased ContentClientCallBack_Type;

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      ControlRegionNetworkImplementation
        :=Network.Config.FindImplementation
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control<->Element.Network"));

      ControlRegionNetworkImplementation.Streams.Initialize.all;

      ContentClient
        :=ControlRegionNetworkImplementation.Streams.NewClient
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String("Control.ContentClient.Network")).all);

      ContentClient.CallBack:=ContentClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      ControlRegionNetworkImplementation.Streams.FreeClient
        (Item => ContentClient);

      ControlRegionNetworkImplementation.Streams.Finalize.all;
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return boolean is
   begin
      ProcessLoop.Process;
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimRegion;
