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
--   20.Apr 2012 Julian Schutsch
--     - Original version

-- Reasons for implementation
--   A common interface for different kinds of middle-ware for distributed
--   systems.

-- Usage
--   Each Distributed System implementation must provide two independent
--   sets of functions:
--    * Functions to spawn processes into a distributed system
--    * Functions for communication among processes in a distributed system
--
--  ### Spawn ###
--
--  Spawning processes is accomplished by creating a SpawnObject with
--  CreateSpawnObject.
--  The SpawnObject provides three callbacks. The OnMessage callback is called
--  on any message written by either the distributed system middle-ware or the
--  individual nodes. OnFailure is called with SupplementConfig which can
--  be used to prompt the user for additional data.
--  OnSuccess is called if the nodes are successfully started. This does not
--  imply success running the nodes.
--
--  The Spawn process itself is triggered when calling Execute. Execute can
--  be called again after OnFailure was triggered.
--
--  ### Node Communication ###
--
--  Communication between nodes is only possible if they are started as nodes
--  through a distributed system middle-ware.
--  Before and after any communication InitializeNode and FinalizeNode must be
--  called.
--
--  The Communication requires periodic calls to ProcessMessages to check
--  for incoming messages as well to send messages.
--  Message callbacks are therefore only to be expected during a
--  ProcessMessages call. If the callback parameter for ProcessMessages
--  is null, an exception ReceiveWithNullCallBack is raised if a message is
--  received. The ownership of packets given by the callback's parameter
--  remains with the distributed systems implementation.
--  The SendMessages call can either add the packet to the buffer or
--  send it immediately. This is up to the actual implementation.

pragma Ada_2005;

with Config;
with Config.Implementations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with Network.Packets;

package DistributedSystems is

   FailedNodeInitialization : Exception;
   ReceiveWithNullCallBack  : Exception;
   InternalError            : Exception;

   type Group_Type is new Integer;
   type Node_Type is new Integer;

   GlobalGroup   : Group_Type;
   MyGlobalID    : Node_Type;
   FirstGlobalID : Node_Type;
   LastGlobalID  : Node_Type;

   type MessageCallBack_Access is
     access procedure
       (Source : Node_Type;
        Packet : Network.Packets.Packet_Access);

   type InitializeNode_Access is
     access procedure
       (Configuration : out Config.Config_Type);

   type FinalizeNode_Access is
     access procedure;

   type Executable_Type is
      record
         Executable : Unbounded_String;
         Amount     : Positive;
      end record;

   type ExecutableArray_Type is array (Integer range <>) of Executable_Type;

   type OnMessage_Access is
     access procedure
       (Message : Unbounded_String);

   type OnFailure_Access is
     access procedure
       (SupplementConfig : Config.Config_Type);

   type OnSuccess_Access is
     access procedure;

   type Spawn_Type is new AnyObject_Type with
      record
         OnMessage : OnMessage_Access:=null;
         OnFailure : OnFailure_Access:=null;
         OnSuccess : OnSuccess_Access:=null;
      end record;

   type Spawn_Access is access all Spawn_Type;
   type Spawn_ClassAccess is access all Spawn_Type'Class;

   -- Execution of all the nodes selected when creating the Spawn Object
   -- (by Executables parameter)
   -- The SupplementConfig can be empty if Execute is called first.
   -- In case of failure, the SupplementConfig may contain a list
   -- of necessary options+default values which may enable Execute to
   -- proceed.
   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type);

   procedure Free
     (Item : access Spawn_Type);

   type CreateSpawnObject_Access is
     access procedure
       (Configuration : Config.Config_Type;
        Executables   : ExecutableArray_Type;
        SpawnObject   : out Spawn_ClassAccess);

   type SendMessage_Access is
     access procedure
       (Node   : Node_Type;
        Packet : Network.Packets.Packet_Access);

   type ProcessMessages_Access is
     access procedure
       (CallBack : MessageCallBack_Access);

   type Implementation_Type is
      record
         CreateSpawnObject : CreateSpawnObject_Access := null;
         InitializeNode    : InitializeNode_Access    := null;
         FinalizeNode      : FinalizeNode_Access      := null;
         SendMessage       : SendMessage_Access       := null;
         ProcessMessages   : ProcessMessages_Access   := null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       =>
      To_Unbounded_String("DistributedSystemsImplementation"));

end DistributedSystems;
