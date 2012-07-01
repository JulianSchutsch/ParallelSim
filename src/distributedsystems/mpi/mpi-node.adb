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

-- Implementation details
--
--   MPI Sending and Receiving
--
--   All Sending and Receiving is non blocking using MPI_Irecv and MPI_Isend.
--   A finite number of receive and send slots is maintained which can
--   receive or send at the same time.
--   MPI_Test is used on each active request wether or not sending/receiving
--   is complete.
--
--   If possible, MPI_Isend is called immediately in SendMessage if a slot
--   is available. Otherwise the packet is added to a fifo queue.
--   ProcessMessages is responsible for checking if another message can be
--   send.

pragma Ada_2005;

with MPI; use MPI;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;
with Interfaces.C;
with Config;
with DistributedSystems; use DistributedSystems;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
with Processes;
with GNAT.Regpat;
with Packets;
with Types;
with Errors;
with Endianess; use Endianess;
with ByteOperations; use ByteOperations;

package body MPI.Node is

   BufferSize : constant := 128*1024;

   type SendSlot_Type is
      record
         Request : aliased MPI.MPI_Request_Type;
         Buffer  : ByteOperations.ByteArray_Access:=null;
         Packet  : Packets.Packet_ClassAccess:=null;
      end record;

   type ReceiveSlot_Type is
      record
         Request : aliased MPI.MPI_Request_Type;
         Buffer  : ByteOperations.ByteArray_Access:=null;
      end record;

   type NodeStatus_Type is
      record
         Packet : Packets.Packet_ClassAccess:=null;
      end record;

   type ReceiveSlotArray_Type is array (Integer range <>) of ReceiveSlot_Type;
   type ReceiveSlotArray_Access is access ReceiveSlotArray_Type;
   type SendSlotArray_Type is array (Integer range <>) of SendSlot_Type;
   type SendSlotArray_Access is access SendSlotArray_Type;
   type NodeStatusArray_Type is array (Integer range <>) of NodeStatus_Type;
   type NodeStatusArray_Access is access NodeStatusArray_Type;

   -- Packets are added to the end(Last) and taken from the beginning(First)
   QueueFirstPacket : Packets.Packet_ClassAccess := null;
   QueueLastPacket  : Packets.Packet_ClassAccess := null;
   SendSlots        : SendSlotArray_Access          := null;
   ReceiveSlots     : ReceiveSlotArray_Access       := null;
   NodeStats        : NodeStatusArray_Access        := null;

   type Spawn_Type is new DistributedSystems.Spawn_Type with
      record
         Configuration       : Config.Config_Type;
         ExecutableArguments : Unbounded_String;
         Process             : aliased Processes.Process_Type;
         SuccessReported     : Boolean:=False;
      end record;
   type Spawn_Access is access all Spawn_Type;

   overriding
   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type);

   overriding
   procedure Free
     (Item : access Spawn_Type);
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access Spawn_Type) is
   begin
      Item.Process.Kill;
      DistributedSystems.Spawn_Access(Item).Free;
   end Free;
   ---------------------------------------------------------------------------

   procedure ProcessTerminate
     (CallBackObject : AnyObject_ClassAccess) is

      Spawn : constant Spawn_Access:=Spawn_Access(CallBackObject);

   begin

      Put_Line("ProcessTerminate");

      if Spawn.OnTerminate/=null then
         Spawn.OnTerminate.all;
      end if;

   end ProcessTerminate;
   ---------------------------------------------------------------------------

   procedure ProcessMessage
     (CallBackObject : AnyObject_ClassAccess;
      Message        : Unbounded_String) is

      Spawn            : constant Spawn_Access:=Spawn_Access(CallBackObject);
      SupplementConfig : Config.Config_Type;

   begin
      if Spawn.OnMessage/=null then
         Spawn.OnMessage(Message);
      end if;
      if GNAT.Regpat.Match
        (Expression => "Credentials required to connect",
         Data       => To_String(Message)) then
         SupplementConfig.Include
           (Key      => U("Account"),
            New_Item => U(""));
         SupplementConfig.Include
           (Key      => U("Password"),
            New_Item => U(""));
         if Spawn.OnFailure/=null then
            Spawn.OnFailure
              (Error =>
                 (Error  => U("MPIDISTSYS:MISSING_CREDENTIALS"),
                  Params => null),
               SupplementConfig => SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "Aborting",
         Data       => To_String(Message)) then
         if Spawn.OnFailure/=null then
            Spawn.OnFailure
              (Error =>
                 (Error  => U("MPIDISTSYS:FAILED EXEC"),
                  Params => null),
               SupplementConfig => SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "launch failed",
         Data       => To_String(Message)) then
         if Spawn.OnFailure/=null then
            Spawn.OnFailure
              (Error =>
                 (Error => U("MPIDISTSYS:FAILED EXEC"),
                  Params => null),
               SupplementConfig => SupplementConfig);
         end if;
         Spawn.Process.Kill;
      end if;
      if GNAT.Regpat.Match
        (Expression => "MPI-Node initialized",
         Data       =>To_String(Message)) then
         if not Spawn.SuccessReported and
           Spawn.OnSuccess/=null then
            Spawn.OnSuccess.all;
            Spawn.SuccessReported:=True;
         end if;
      end if;
   end ProcessMessage;
   ---------------------------------------------------------------------------

   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : Config.Config_Type) is

      use type StringStringMap_Pack.Cursor;

      Arguments      : Unbounded_String;
      Cursor         : StringStringMap_Pack.Cursor;

   begin

      Item.Configuration.SaveToFile("_MyConfig");

      Arguments := U("-noprompt ");

      Cursor:=SupplementConfig.Find(U("Account"));
      if Cursor/=StringStringMap_Pack.No_Element then
         declare
            Account  : constant Unbounded_String:=StringStringMap_Pack.Element(Cursor);
            Password : Unbounded_String;
            File     : File_Type;
         begin
            Cursor:=SupplementConfig.Find(U("Password"));
            if Cursor/=StringStringMap_Pack.No_Element then
               Password:=StringStringMap_Pack.Element(Cursor);
               begin
                  Delete_File("MPIEXEC.PWD");
               exception
                  when others =>
                     null;
               end;
               Create
                 (File => File,
                  Mode => Out_File,
                  Name => "MPIEXEC.PWD");
               Put_Line(File,To_String(Account));
               Put_Line(File,To_String(Password));
               Close(File);
               Arguments:=Arguments&"-pwdfile MPIEXEC.PWD ";
            end if;
         end;
      end if;

      Arguments := Arguments&Item.ExecutableArguments;

      if Item.OnMessage/=null then
         Item.OnMessage("Running mpiexec with "&Arguments);
      end if;

      Item.Process.CallBackObject:=AnyObject_ClassAccess(Item);
      Item.Process.OnMessage   := ProcessMessage'Access;
      Item.Process.OnTerminate := ProcessTerminate'Access;
      begin
         Item.Process.Execute
           (ProgramName => U("mpiexec"),
            Arguments   => Arguments);
      exception
         when Processes.ExecutableNotFound =>
            Item.OnFailure
              (Error =>
                 (Error  => U("MPIDISTSYS:!MPIEXEC"),
                  Params => null),
               SupplementConfig => SupplementConfig);
         when Processes.FailedToCreatePipe =>
            Item.OnFailure
              (Error  =>
                 (Error => U("MPIDISTSYS:!MPIEXEC.PIPE"),
                  Params => null),
               SupplementConfig => SupplementConfig);
         when E:Processes.FailedExecute =>
            Item.OnFailure
              (Error =>
                 (Error  => U("MPIDISTSYS:!MPIEXEC.EXEC"),
                  Params => Errors.StringToParams(Ada.Exceptions.Exception_Message(E))),
               SupplementConfig => SupplementConfig);
      end;

      if Item.OnMessage/=null then
         Item.OnMessage(U("Output from mpiexec:"));
      end if;

   end Execute;
   ---------------------------------------------------------------------------

   procedure CreateSpawnObject
     (Configuration : Config.Config_Type;
      Executables   : ExecutableArray_Type;
      SpawnObject   : out Spawn_ClassAccess) is

      Spawn : Spawn_Access;

   begin

      Spawn:=new Spawn_Type;
      Spawn.Configuration:=Configuration;

      for i in Executables'Range loop
         Spawn.ExecutableArguments:=Spawn.ExecutableArguments& "-n"
           &Positive'Image(Executables(i).Amount)&" "
           &Processes.CurrentPathPrefix&Executables(i).Executable
           &" -DistributedSystemsImplementation=MPI";
         if i/=Executables'Last then
            Spawn.ExecutableArguments:=Spawn.ExecutableArguments&" : ";
         end if;

      end loop;
      SpawnObject:=Spawn_ClassAccess(Spawn);

   end CreateSpawnObject;
   ---------------------------------------------------------------------------

   procedure InitializeNode
     (Configuration : out Config.Config_Type) is

      use type Interfaces.C.int;

      WorldRank : aliased Interfaces.C.int;
      WorldSize : aliased Interfaces.C.int;
      Result    : Interfaces.C.int;

   begin

      Configuration.LoadFromFile
        (FileName => "_MyConfig");
      MPI_Init
        (Argc => null,
         Args => null);
      ------------------------------------------------------------------------

      Result:=MPI_Comm_Rank
        (comm => MPI_COMM_WORLD,
         rank => WorldRank'Access);

      if Result/=MPI_SUCCESS then
         raise FailedNodeInitialization
           with "Call to MPI_Comm_Rank failed with "
             &ErrorToString(Result);
      end if;

      Result:=MPI_Comm_Size
        (comm => MPI_COMM_WORLD,
         size => WorldSize'Access);

      if Result/=MPI_Success then
         raise FailedNodeInitialization
           with "Call to MPI_COMM_Size failed with "
             &ErrorToString(Result);
      end if;

      ThisNode     := Node_Type(WorldRank);
      FirstNode    := 0;
      LastNode     := Node_Type(WorldSize-1);
      GlobalGroup  := DistributedSystems.Group_Type(MPI_COMM_WORLD);
      NodeCount    := Integer(WorldSize);
      ------------------------------------------------------------------------

      SendSlots    := new SendSlotArray_Type(0..0);
      ReceiveSlots := new ReceiveSlotArray_Type(0..0);
      NodeStats    := new NodeStatusArray_Type
        (Integer(FirstNode)..Integer(LastNode));

      for i in SendSlots'Range loop
         SendSlots(i).Buffer:=new ByteOperations.ByteArray_Type(0..BufferSize-1);
      end loop;

      for i in ReceiveSlots'Range loop
         declare
            Slot : ReceiveSlot_Type renames ReceiveSlots(i);
         begin
            Slot.Buffer:=new ByteOperations.ByteArray_Type(0..BufferSize-1);
            Result:=MPI_Irecv
              (buf      => Slot.Buffer(0)'Access,
               count    => BufferSize,
               datatype => MPI.MPI_BYTE,
               source   => MPI.MPI_ANY_SOURCE,
               tag      => MPI.MPI_ANY_TAG,
               comm     => MPI.MPI_COMM_WORLD,
               request  => Slot.Request'Access);
            if Result/=MPI_SUCCESS then
               raise FailedNodeInitialization
                 with "Call to MPI_Irecv for receive preinitialisation failed with "
                   &ErrorToString(Result);
            end if;
         end;
      end loop;
      ------------------------------------------------------------------------

      Put_Line("MPI-Node initialized "&Node_Type'Image(ThisNode));

   end InitializeNode;
   ---------------------------------------------------------------------------

   procedure FinalizeNode is

      use type Packets.Packet_ClassAccess;

   begin

      declare
         Packet     : Packets.Packet_ClassAccess;
         NextPacket : Packets.Packet_ClassAccess;
      begin

         Packet:=QueueFirstPacket;
         while Packet/=null loop

            NextPacket:=Packet.Next;
            Packets.Free(Packet);
            Packet:=NextPacket;

         end loop;

         QueueFirstPacket:=null;
         QueueLastPacket:=null;

      end;
      ------------------------------------------------------------------------

      for i in SendSlots'Range loop

         if SendSlots(i).Packet/=null then
            Packets.Free(SendSlots(i).Packet);
         end if;

         ByteOperations.Free(SendSlots(i).Buffer);

      end loop;

      for i in ReceiveSlots'Range loop

         ByteOperations.Free(ReceiveSlots(i).Buffer);

      end loop;
      ------------------------------------------------------------------------

      for i in NodeStats'Range loop

         if NodeStats(i).Packet/=null then
            Packets.Free(NodeStats(i).Packet);
         end if;

      end loop;

      MPI_Finalize;
      ------------------------------------------------------------------------

   end FinalizeNode;
   ---------------------------------------------------------------------------

   -- Tries to send a packet using a free slot.
   -- Returns True if  a slot is free
   -- Does no Queue handling!
   function SendPacket
     (Packet : Packets.Packet_ClassAccess)
      return Boolean is

      use type Packets.Packet_ClassAccess;
      use type Interfaces.C.int;

      Error : Interfaces.C.int;

   begin

--      Put_Line("Scanning Slots"&Node_Type'Image(ThisNode));

      for i in SendSlots'Range loop

         declare
            Slot : SendSlot_Type renames SendSlots(i);
         begin

            if Slot.Packet=null then

               Slot.Packet := Packet;
               ByteAccessToLECardinal32Access(Slot.Buffer(0)'Access).all
                 :=To(Types.Cardinal32(Slot.Packet.Amount));

               Slot.Packet.Position:=Integer'Min
                 (BufferSize-Types.Cardinal32'Size/8,
                  Slot.Packet.Amount);

               Put_Line("Sending Packet from "&Node_Type'Image(ThisNode)
                          &" [0.."&Integer'Image(Slot.Packet.Position-1)&"]");

               Slot.Buffer
                 (Types.Cardinal32'Size/8..Slot.Packet.Position+Types.Cardinal32'Size/8-1)
                 :=Slot.Packet.Content(0..Slot.Packet.Position-1);

               Slot.Packet:=Packet;

               Error:=MPI_Isend
                 (buf      => Slot.Buffer(0)'Access,
                  count    => Interfaces.C.int(Slot.Packet.Position+Types.Cardinal32'Size/8),
                  datatype => MPI_BYTE,
                  dest     => Packet.CData1,
                  tag      => 0,
                  comm     => MPI_COMM_WORLD,
                  request  => Slot.Request'Access);
               if Error/=MPI_SUCCESS then
                  raise InternalError with "MPI_Isend (sendpacket)failed with "
                    &ErrorToString(Error);
               end if;

               return True;

            end if;
         end;
      end loop;
      return False;
   end SendPacket;
   ---------------------------------------------------------------------------

   procedure ProcessMessages
     (CallBack : MessageCallBack_Access) is

      use type Packets.Packet_ClassAccess;
      use type Interfaces.C.int;

      Result : Interfaces.C.int;
      Flag   : aliased Interfaces.C.int;
      Status : aliased MPI.MPI_Status_Type;

   begin

      for i in ReceiveSlots'Range loop

         declare
            Slot : ReceiveSlot_Type renames ReceiveSlots(i);
         begin

            Result:=MPI_Test
              (request => Slot.Request'Access,
               flag    => Flag'Access,
               status  => Status'Access);
            if Result/=MPI_SUCCESS then
               raise InternalError with "MPI_Test failed with "
                 &ErrorToString(Result);
            end if;

            if Flag/=0 then

               declare
                  Node : NodeStatus_Type renames NodeStats
                    (Integer(Status.MPI_SOURCE));
               begin

                  if Node.Packet=null then

                     Node.Packet        := new Packets.Packet_Type;
                     declare
                        NewPacketSize : Types.Cardinal32;
                     begin
                        NewPacketSize:=From
                          (ByteAccessToLECardinal32Access(Slot.Buffer(0)'Access).all);

                        Node.Packet.Content:=new ByteArray_Type
                          (0..Integer(NewPacketSize)-1);
                        Node.Packet.Position:=Integer'Min
                          (BufferSize-Types.Cardinal32'Size/8,
                           Integer(NewPacketSize));
                        Put_Line("Receive Packet from "&
                                 Integer'Image(Integer(Status.MPI_SOURCE))
                                 &" [0.."&Integer'Image(Node.Packet.Position-1)&"] max "
                                  &Integer'Image(Node.Packet.Amount));
                        Node.Packet.Content
                          (0..Node.Packet.Position-1)
                          :=Slot.Buffer(Types.Cardinal32'Size/8..
                                          Types.Cardinal32'Size/8+Node.Packet.Position-1);
                        Node.Packet.Amount:=Integer(NewPacketSize);
                     end;

                  else

                     declare
                        NewPos : Integer;
                     begin
                        NewPos:=Integer'Min
                          (Node.Packet.Position+BufferSize,
                           Node.Packet.Amount);
                        Put_Line("Receive Part from "&
                                 Integer'Image(Integer(Status.MPI_SOURCE))
                                 &" ["&Integer'Image(Node.Packet.Position)&".."
                                 &Integer'Image(NewPos-1)&"] max "
                                 &Integer'Image(Node.Packet.Amount));
                        Node.Packet.Content
                          (Node.Packet.Position..NewPos-1)
                          :=Slot.Buffer(0..NewPos-Node.Packet.Position-1);
                        Node.Packet.Position:=NewPos;
                     end;

                  end if;

                  if  Node.Packet.Position=Node.Packet.Amount then
                     Node.Packet.Position:=0;
                     CallBack
                       (Source => Node_Type(Status.MPI_SOURCE),
                        Packet => Node.Packet);
                     Packets.Free(Node.Packet);
                     Node.Packet:=null;
                  end if;

               end;

               Result:=MPI.MPI_Irecv
                 (buf => Slot.Buffer(0)'Access,
                  count => BufferSize,
                  datatype => MPI.MPI_BYTE,
                  source => MPI.MPI_ANY_SOURCE,
                  tag => MPI.MPI_ANY_TAG,
                  comm => MPI.MPI_COMM_WORLD,
                  request => Slot.Request'Access);
               if Result/=MPI.MPI_SUCCESS then
                  raise InternalError with "MPI_Irecv (continue) failed with "
                    &ErrorToString(Result);
               end if;

            end if;

         end;

      end loop;

      -- Check if packets are in the send queue
      while QueueFirstPacket/=null loop

         if SendPacket(QueueFirstPacket) then

            QueueFirstPacket:=QueueFirstPacket.Next;

            if QueueFirstPacket/=null then
               QueueFirstPacket.Last:=null;
            else
               QueueLastPacket:=null;
            end if;

         else

            exit;

         end if;

      end loop;

      for i in SendSlots'Range loop
         declare
            Slot   : SendSlot_Type renames SendSlots(i);
            Flag   : aliased Interfaces.C.int;
            Status : aliased MPI.MPI_Status_Type;
         begin

            if Slot.Packet/=null then

               Result:=MPI.MPI_Test
                 (request => Slot.Request'Access,
                  flag    => Flag'Access,
                  status  => Status'Access);
               if Result/=MPI_SUCCESS then
                  raise InternalError with "MPI_Test (SendSlots check in Processmessages failed with "
                    &ErrorToString(Result);
               end if;

               Put_Line("Send Stat "&Interfaces.C.int'Image(Flag));

               if Flag/=0 then
                  if Slot.Packet.Position=Slot.Packet.Amount then
                     Put_Line("Send Complete"&Node_Type'Image(ThisNode));
                     Packets.Free(Slot.Packet);
                     Slot.Packet:=null;
                  else
                     Put_Line("Send Cont"&Node_Type'Image(ThisNode));
                     declare
                        NewPos : Integer;
                     begin
                        NewPos:=Integer'Min
                          (Slot.Packet.Position+BufferSize,
                           Slot.Packet.Amount);
--                        Put_Line("Send Part from "&Node_Type'Image(ThisNode)
--                          &"["&Integer'Image(Slot.Packet.Position)&".."
--                                 &Integer'Image(NewPos-1)&"] max"&Integer'Image(Slot.Packet.Amount));
                        Slot.Buffer(0..NewPos-Slot.Packet.Position-1)
                          :=Slot.Packet.Content(Slot.Packet.Position..
                                                  NewPos-1);
                        Result:=MPI.MPI_Isend
                          (buf      => Slot.Buffer(0)'Access,
                           count    => Interfaces.C.int(NewPos-Slot.Packet.Position),
                           datatype => MPI_BYTE,
                           dest     => Slot.Packet.CData1,
                           tag      => 0,
                           comm     => MPI_COMM_WORLD,
                           request  => Slot.Request'Access);
                        if Result/=MPI_SUCCESS then
                           raise InternalError with "MPI_Isend (Processmessages) failed with "
                             &ErrorToString(Result);
                        end if;
                        Slot.Packet.Position:=NewPos;
                     end;
--                     Put_Line("Send Part//");
                  end if;
               end if;
            end if;
         end;
      end loop;

   end ProcessMessages;
   ---------------------------------------------------------------------------

   procedure  WaitForSend is
      use type Packets.Packet_ClassAccess;
   begin
      Put_Line("WaitForSend phase 1");
      while QueueFirstPacket/=null loop
         ProcessMessages(null);
      end loop;
      Put_Line("WaitForSend phase 2");
      loop
         declare
            SlotUsed : Boolean:=False;
         begin
            SlotTestLoop:
            for i in SendSlots'Range loop
               if (SendSlots(i).Packet/=null) then
                  SlotUsed:=True;
                  exit SlotTestLoop;
               end if;
            end loop SlotTestLoop;
            if not SlotUsed then
               exit;
            end if;
         end;
         ProcessMessages(null);
      end loop;
   end WaitForSend;
   ---------------------------------------------------------------------------

   procedure SendMessage
     (Dest   : Node_Type;
      Packet : Packets.Packet_ClassAccess) is

      use type Packets.Packet_ClassAccess;

   begin

      Packet.CData1:=Interfaces.C.int(Dest);
      if not SendPacket(Packet) then
         Packet.Last:=QueueLastPacket;
         Packet.Next:=null;
         if QueueLastPacket/=null then
            QueueLastPacket.Next:=Packet;
         else
            QueueFirstPacket:=Packet;
         end if;
         QueueLastPacket:=Packet;
      end if;

   end SendMessage;
   ---------------------------------------------------------------------------

   function GetTime
     return Long_Float is
   begin
      return Long_Float(MPI.MPI_Wtime);
   end GetTime;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (InitializeNode    => InitializeNode'Access,
      FinalizeNode      => FinalizeNode'Access,
      CreateSpawnObject => CreateSpawnObject'Access,
      ProcessMessages   => ProcessMessages'Access,
      SendMessage       => SendMessage'Access,
      WaitForSend       => WaitForSend'Access,
      GetTime           => GetTime'Access);
   Identifier : constant Unbounded_String:=U("MPI");

   procedure Register is
   begin

      Implementations.Register
        (Implementation => Implementation,
         Identifier     => Identifier);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister
        (Identifier => Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end MPI.Node;
