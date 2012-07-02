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
--   2.Jul 2012 Julian Schutsch
--     - Original version

with Network.Streams;
with Network.UseImplementations;
with Config;
with Basics; use Basics;
--with Ada.Text_IO; use Ada.Text_IO;

procedure ManyCon is

   ClientCount : constant := 5;

   Implementation : Network.Streams.Implementation_Type;
--   Server  : Network.Streams.Server_ClassAccess;
   Clients : array(1..ClientCount) of Network.Streams.Client_ClassAccess;

   type ServerCallBack_Type is new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);

   type ClientCallBack_Type is new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure Connect
     (Item : in out ClientCallBack_Type);

   procedure AAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
   begin
      null;
   end AAccept;
   ---------------------------------------------------------------------------

   procedure Connect
     (Item : in out ClientCallBack_Type) is
   begin
      null;
   end Connect;
   ---------------------------------------------------------------------------

   Configuration : Config.Config_Type;

begin

   Configuration.Insert(U(".Family"),U("IPv4"));
   Configuration.Insert(U(".BindIP"),U("127.0.0.1"));
   Configuration.Insert(U(".BindPort"),U("10000"));
   Configuration.Insert(U(".RemoteIP"),U("127.0.0.1"));
   Configuration.Insert(U(".RemotePort"),U("10000"));

   Network.UseImplementations.Register;
   Implementation:=Network.Streams.Implementations.FindAny;

--   Implementation.Initialize.all;

--   Put_Line("Create Server");
--   Server:=Implementation.NewServer(Configuration,U(""));

   for i in Clients'Range loop
--      Put_Line("Create Client "&Integer'Image(i));
      Clients(i):=Implementation.NewClient(Configuration,U(""));
   end loop;
   -- Wait for all this to happen

   for i in Clients'Range loop
--      Put_Line("Destroy Client "&Integer'Image(i));
      Implementation.FreeClient(Clients(i));
   end loop;

--   Put_Line("Destroy Server");
--   Implementation.FreeServer(Server);

--   Put_Line("Finalize");
--   Implementation.Finalize.all;

--   Network.UseImplementations.Unregister;

end ManyCon;
------------------------------------------------------------------------------
