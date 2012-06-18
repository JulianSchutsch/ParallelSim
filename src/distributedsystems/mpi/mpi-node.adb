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
with Network.Packets;
with Types;
with Errors;
with Endianess; use Endianess;
with ByteOperations; use ByteOperations;

package body MPI.Node is

   BufferSize : constant := 16*4096;

   type Slot_Type is
      record
         Receiving : Boolean:=False;
         Request   : aliased MPI.MPI_Request_Type;
         Buffer    : ByteOperations.ByteArray_Access:=null;
         Packet    : Network.Packets.Packet_Access:=null;
      end record;

   type SlotArray_Type is array (Integer range <>) of Slot_Type;
   type SlotArray_Access is access SlotArray_Type;

   -- Packets are added to the end(Last) and taken from the beginning(First)
   QueueFirstPacket : Network.Packets.Packet_Access := null;
   QueueLastPacket  : Network.Packets.Packet_Access := null;
   SendSlots        : SlotArray_Access := null;
   ReceiveSlots     : SlotArray_Access := null;

   type Spawn_Type is new DistributedSystems.Spawn_Type with
      record
         Configuration       : Config.Config_Type;
         ExecutableArguments : Unbounded_String;
         Process             : aliased Processes.Process_Type;
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
         if Spawn.OnSuccess/=null then
            Spawn.OnSuccess.all;
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
      Item.Process.OnMessage:=ProcessMessage'Access;
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

      MyGlobalID    := Node_Type(WorldRank);
      FirstGlobalID := 0;
      LastGlobalID  := Node_Type(WorldSize-1);
      GlobalGroup   := DistributedSystems.Group_Type(MPI_COMM_WORLD);
      ------------------------------------------------------------------------

      SendSlots    := new SlotArray_Type(0..2);
      ReceiveSlots := new SlotArray_Type(0..2);

      for i in SendSlots'Range loop
         SendSlots(i).Buffer:=new ByteOperations.ByteArray_Type(0..BufferSize-1);
      end loop;

      for i in ReceiveSlots'Range loop
         ReceiveSlots(i).Buffer:=new ByteOperations.ByteArray_Type(0..BufferSize-1);
      end loop;
      ------------------------------------------------------------------------

      Put_Line("MPI-Node initialized "&Node_Type'Image(MyGlobalID));

   end InitializeNode;
   ---------------------------------------------------------------------------

   procedure FinalizeNode is

      use type Network.Packets.Packet_Access;

   begin

      declare
         Packet     : Network.Packets.Packet_Access;
         NextPacket : Network.Packets.Packet_Access;
      begin

         Packet:=QueueFirstPacket;
         while Packet/=null loop

            NextPacket:=Packet.Next;
            Network.Packets.Free(Packet);
            Packet:=NextPacket;

         end loop;

         QueueFirstPacket:=null;
         QueueLastPacket:=null;

      end;
      ------------------------------------------------------------------------

      for i in SendSlots'Range loop

         if SendSlots(i).Packet/=null then
            Network.Packets.Free(SendSlots(i).Packet);
         end if;

         ByteOperations.Free(SendSlots(i).Buffer);

      end loop;

      for i in ReceiveSlots'Range loop

         if ReceiveSlots(i).Packet/=null then
            Network.Packets.Free(ReceiveSlots(i).Packet);
         end if;

         ByteOperations.Free(ReceiveSlots(i).Buffer);

      end loop;
      ------------------------------------------------------------------------

      MPI_Finalize;
      ------------------------------------------------------------------------

   end FinalizeNode;
   ---------------------------------------------------------------------------

   -- Tries to send a packet using a free slot.
   -- Returns True if  a slot is free
   -- Does no Queue handling!
   function SendPacket
     (Packet : Network.Packets.Packet_Access)
      return Boolean is

      use type Network.Packets.Packet_Access;
      use type Interfaces.C.int;

      Error : Interfaces.C.int;

   begin
      for i in SendSlots'Range loop

         declare
            Slot : Slot_Type renames SendSlots(i);
         begin

            if Slot.Packet=null then

               Slot.Packet := Packet;
               ByteAccessToLECardinal32Access(Slot.Buffer(0)'Access).all
                 :=To(Types.Cardinal32(Slot.Packet.Amount));

               Slot.Packet.Position:=Integer'Min
                 (BufferSize-Types.Cardinal32'Size/8,
                  Slot.Packet.Amount);

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

      use type Network.Packets.Packet_Access;
      use type Interfaces.C.int;

      Error  : Interfaces.C.int;
      Flag   : aliased Interfaces.C.int;
      Status : aliased MPI.MPI_Status_Type;

   begin

      -- Check for incoming messages
      for i in ReceiveSlots'Range loop
         declare
            Slot : Slot_Type renames ReceiveSlots(i);
         begin
            if not Slot.Receiving then
               Error:=MPI_Irecv
                 (buf      => Slot.Buffer(ReceiveSlots(i).Buffer'First)'Access,
                  count    => Slot.Buffer'Length,
                  datatype => MPI.MPI_BYTE,
                  source   => MPI.MPI_ANY_SOURCE,
                  tag      => 0,
                  comm     => MPI.MPI_COMM_WORLD,
                  request  => Slot.Request'Access);
               if Error/=MPI_SUCCESS then
                  raise InternalError with "MPI_Irecv failed with "
                    &ErrorToString(Error);
               end if;

               Slot.Receiving:=True;

            else

               Error:=MPI_Test
                 (request => Slot.Request'Access,
                  flag    => Flag'Access,
                  status  => Status'Access);
               if Error/=MPI_SUCCESS then
                  raise InternalError with "MPI_Test failed with "
                    &ErrorToString(Error);
               end if;

               if Flag/=0 then

                  if Slot.Packet=null then

                     Slot.Packet        := new Network.Packets.Packet_Type;
                     Slot.Packet.CData1 := Status.MPI_SOURCE;

                     declare
                        NewPacketSize : Types.Cardinal32;
                     begin
                        NewPacketSize:=From
                          (ByteAccessToLECardinal32Access(Slot.Buffer(0)'Access).all);

                        Slot.Packet.Content:=new ByteArray_Type
                          (0..Integer(NewPacketSize)-1);
                        Slot.Packet.Position:=Integer'Min
                          (BufferSize-Types.Cardinal32'Size,
                           Integer(NewPacketSize));
                        Slot.Packet.Content
                          (0..Slot.Packet.Position-1)
                          :=Slot.Buffer(Types.Cardinal32'Size/8..
                                          Types.Cardinal32'Size/8+Slot.Packet.Position-1);
                        Slot.Packet.Amount:=Integer(NewPacketSize);
                     end;
                  else
                     declare
                        NewPos : Integer;
                     begin
                        NewPos:=Integer'Min
                          (Slot.Packet.Position+BufferSize,
                           Slot.Packet.Amount);
                        Slot.Packet.Content
                          (Slot.Packet.Position..NewPos)
                          :=Slot.Buffer(0..NewPos-Slot.Packet.Position);
                        Slot.Packet.Position:=NewPos;
                     end;

                  end if;

                  if  Slot.Packet.Position=Slot.Packet.Amount then
                     Slot.Packet.Position:=0;
                     CallBack
                       (Source => Node_Type(Slot.Packet.CData1),
                        Packet => Slot.Packet);
                     Network.Packets.Free(Slot.Packet);
                     Slot.Packet:=null;
                     Slot.Receiving:=False;
                     --TODO: Insert Irecv call here.
                  end if;

               end if;

            end if;

         end;

      end loop;

      -- Check if packets are in the send queue
      while QueueFirstPacket/=null loop

         if SendPacket(QueueFirstPacket) then

            declare
               Packet : Network.Packets.Packet_Access;
            begin
               Packet:=QueueFirstPacket;
               QueueFirstPacket:=QueueFirstPacket.Next;
               if QueueFirstPacket/=null then
                  QueueFirstPacket.Last:=null;
               else
                  QueueLastPacket:=null;
               end if;
               Network.Packets.Free(Packet);
            end;

         end if;

      end loop;

   end ProcessMessages;
   ---------------------------------------------------------------------------

   procedure SendMessage
     (Node   : Node_Type;
      Packet : Network.Packets.Packet_Access) is

      use type Network.Packets.Packet_Access;

   begin

      Packet.CData1:=Interfaces.C.int(Node);
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

   Implementation : constant Implementation_Type:=
     (InitializeNode    => InitializeNode'Access,
      FinalizeNode      => FinalizeNode'Access,
      CreateSpawnObject => CreateSpawnObject'Access,
      ProcessMessages   => ProcessMessages'Access,
      SendMessage       => SendMessage'Access);
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
