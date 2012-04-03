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

with Ada.Calendar;
with Network.Streams;
with ProcessLoop;
with Logging;
with Ada.Streams;
with SimCommon;
with TaskQueue;
with AdminProtocol;
with Endianess;

package body SimAdmin is

   type SendStatus_Enum is
     (SendStatusIdentify,
      SendStatusWaitForIdentification,
      SendStatusReady,
      SendStatusProcessJob);

   type ReceiveStatus_Enum is
     (ReceiveStatusSendingIdentification,
      ReceiveStatusWaitForIdentification,
      ReceiveStatusWaitForCommand);

   type ClientCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with null record;

   overriding
   procedure OnConnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnFailedConnect
     (Item : in out ClientCallBack_Type;
      Retry : in out Boolean);

   overriding
   procedure OnDisconnect
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ClientCallBack_Type);

   overriding
   procedure OnCanSend
     (Item : in out ClientCallBack_Type);
   ---------------------------------------------------------------------------

   type MsgMessage_Type is new TaskQueue.Task_Type with
      record
         Message : Unbounded_String;
      end record;
   type MsgMessage_Access is access MsgMessage_Type;
   overriding
   function Execute
     (Item      : access MsgMessage_Type)
      return Boolean;

   type MsgShutdown_Type is new TaskQueue.Task_Type with null record;
   type MsgShutdown_Access is access MsgShutdown_Type;
   overriding
   function Execute
     (Item : access MsgShutdown_Type)
      return Boolean;


   StreamImplementation : Network.Streams.Implementation_Type;
   Client               : Network.Streams.Client_ClassAccess;
   LogImplementation    : Logging.Implementation_Type;
   LogContext           : Logging.Context_ClassAccess;
   LogChannel           : Logging.Channel_ClassAccess;
   SendStatus           : SendStatus_Enum;
   ReceiveStatus        : ReceiveStatus_Enum;
   ClientCallBack       : aliased ClientCallBack_Type;
   Connected            : Boolean:=False;
   SendQueue            : aliased TaskQueue.Queue_Type;
   CurrentSendJob       : TaskQueue.Task_ClassAccess;

   CurrentSendJobNotFreed    : Exception;
   CurrentSendJobNotAssigned : Exception;

   ---------------------------------------------------------------------------

   function Execute
     (Item : access MsgMessage_Type)
      return Boolean is
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Sending Message...");

      AdminProtocol.ServerCmd_NetworkType'Write
        (Client,
         Endianess.To(AdminProtocol.ServerCmdMessage));

      AdminProtocol.ServerCmd_Msg_NetworkType'Write
        (Client,
         Endianess.To
           (AdminProtocol.ServerCmd_Msg_NativeType(Length(Item.Message))));

      String'Write
        (Client,
         To_String(Item.Message));

      return True;
   end Execute;
   ---------------------------------------------------------------------------

   function Execute
     (Item : access MsgShutdown_Type)
      return Boolean is
      pragma Unreferenced(Item);
   begin
      LogChannel.Write
        (Level => Logging.LevelEvent,
         Message => "Send shutdown message to Admin server");
      AdminProtocol.ServerCmd_NetworkType'Write
        (Client,
         Endianess.To(AdminProtocol.ServerCmdShutdown));
      return True;
   end Execute;
   ---------------------------------------------------------------------------

   procedure OnCanSend
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);

      use type TaskQueue.Task_ClassAccess;

      PrevPosition : Ada.Streams.Stream_Element_Offset;
      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition:=Client.WritePosition;
         case SendStatus is
            when SendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Client,
                  SimCommon.NetworkAdminClientID);
               SendStatus    := SendStatusWaitForIdentification;
               ReceiveStatus := ReceiveStatusWaitForIdentification;
               return;
            when SendStatusWaitForIdentification =>
               return;
            when SendStatusReady =>
               if CurrentSendJob/=null then
                  raise CurrentSendJobNotFreed;
               end if;
               TaskQueue.First
                 (Queue   => SendQueue,
                  Element => CurrentSendJob);
               if CurrentSendJob/=null then
                  SendStatus:=SendStatusProcessJob;
               else
                  return;
               end if;
            when SendStatusProcessJob =>
               if CurrentSendJob=null then
                  raise CurrentSendJobNotAssigned;
               end if;
               if CurrentSendJob.Execute then
                  TaskQueue.FreeTask(CurrentSendJob);
                  CurrentSendJob := null;
                  SendStatus     := SendStatusReady;
               end if;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Client.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnReceive
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);

      use type SimCommon.NetworkIDString;

      PrevPosition : Ada.Streams.Stream_Element_Offset;
      pragma Warnings(Off,PrevPosition);

   begin
      loop
         PrevPosition:=Client.ReceivePosition;
         case ReceiveStatus is
            when ReceiveStatusSendingIdentification =>
               return;
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identifier : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Client,
                     Identifier);
                  if Identifier/=SimCommon.NetworkAdminServerID then
                     LogChannel.Write
                       (Level => Logging.LevelInvalid,
                        Message => "Identification (Admin) send by the server is invalid.");
                     -- TODO: Set ALL TO INVALID
                     return;
                  else
                     LogChannel.Write
                       (Level   => Logging.LevelEvent,
                        Message => "Identification (Admin) send by the server is valid.");
                     SendStatus    := SendStatusReady;
                     ReceiveStatus := ReceiveStatusWaitForCommand;
                  end if;
               end;
            when ReceiveStatusWaitForCommand =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Client.ReceivePosition:=PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnConnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Succeded to connect to Admin server.");
      SendStatus    := SendStatusIdentify;
      ReceiveStatus := ReceiveStatusSendingIdentification;
      Connected:=True;
   end OnConnect;
   ---------------------------------------------------------------------------

   procedure OnFailedConnect
     (Item  : in out ClientCallBack_Type;
      Retry : in out Boolean) is
      pragma Warnings(Off,Item);
   begin
      LogChannel.Write
        (Level   => Logging.LevelFailure,
         Message => "Failed to connect to Admin server.");

      Retry:=True;
   end OnFailedConnect;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ClientCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      Connected:=False;
   end OnDisconnect;
   ---------------------------------------------------------------------------

   procedure WaitForCompletion is
   begin
      while not TaskQueue.Empty(SendQueue) loop
         ProcessLoop.Process;
      end loop;
   end WaitForCompletion;
   ---------------------------------------------------------------------------

   function WaitForConnection
     return Boolean is

      use type Ada.Calendar.Time;

      StartTime : Ada.Calendar.Time;

   begin
      -- Terminate loop after 3 seconds
      StartTime := Ada.Calendar.Clock;
      loop
         ProcessLoop.Process;
         exit when (Ada.Calendar.Clock-StartTime)>15.0;
         exit when Connected;
      end loop;
      return Connected;
   end WaitForConnection;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is
   begin
      LogImplementation:=Logging.Implementations.Find
        (Configuration  => Configuration,
         Node           => To_Unbounded_String("Logging"));
      LogContext:=LogImplementation.NewContext
        (Configuration => Configuration,
         ModuleName => To_Unbounded_String("Admin"));
      LogContext.NewChannel
        (ChannelName => To_Unbounded_String(""),
         Channel     => LogChannel);

      StreamImplementation
        := Network.Streams.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Network"));

      StreamImplementation.Initialize.all;

      Client
        :=StreamImplementation.NewClient
          (Configuration => Configuration,
           Node          => To_Unbounded_String("Admin.Client.Network"));
      Client.CallBack:=ClientCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------
   procedure SendShutdown is
      Msg : MsgShutdown_Access;
   begin
      if not Connected then
         raise NotConnected;
      end if;
      Msg:=new MsgShutdown_Type;
      TaskQueue.AddTask
        (Queue => SendQueue'Access,
         Item  => TaskQueue.Task_ClassAccess(Msg));
   end SendShutdown;
   ---------------------------------------------------------------------------

   procedure SendMessage
     (Message : Unbounded_String) is
      Msg : MsgMessage_Access;
   begin
      if not Connected then
         raise NotConnected;
      end if;
      if Length(Message)>128 then
         raise FailedSend with "Message too long. Must not exceed 128 characters";
      end if;
      Msg:=new MsgMessage_Type;
      Msg.Message:=Message;
      TaskQueue.AddTask
        (Queue => SendQueue'Access,
         Item  => TaskQueue.Task_ClassAccess(Msg));
   end SendMessage;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      StreamImplementation.FreeClient
        (Item => Client);
      StreamImplementation.Finalize.all;

      LogImplementation.FreeContext
        (Item => LogContext);
   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return Boolean is

   begin
      return false;
   end Process;
   ---------------------------------------------------------------------------

end SimAdmin;
