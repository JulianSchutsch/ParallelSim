pragma Ada_2005;

with Network.Streams;
with Ada.Streams;
with SimCommon;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Logging;
with Basics; use Basics;

package body SimControl.ControlServer is

  type ReceiveStatus_Enum is
     (ReceiveStatusWaitForIdentification,
      ReceiveStatusReady,
      ReceiveStatusInvalid);

   type SendStatus_Enum is
     (SendStatusReceive,
      SendStatusIdentify,
      SendStatusReady);

   type ServerChannelCallBack_Type is
     new Network.Streams.ChannelCallBack_Type with
      record
         LogChannel    : Logging.Channel_ClassAccess;
         Channel       : Network.Streams.Channel_ClassAccess;
         ReceiveStatus : ReceiveStatus_Enum;
         SendStatus    : SendStatus_Enum;
      end record;

   type ServerChannelCallBack_Access is
     access all ServerChannelCallBack_Type;

   overriding
   procedure OnCanSend
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type);

   overriding
   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type);
   ---------------------------------------------------------------------------

   type ServerCallBack_Type is
     new Network.Streams.ServerCallBack_Type with null record;

   overriding
   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess);
   ---------------------------------------------------------------------------

   ServerCallBack       : aliased ServerCallBack_Type;
   StreamImplementation : Network.Streams.Implementation_Type;
   Server               : Network.Streams.Server_ClassAccess;
   LogContext           : Logging.Context_ClassAccess;
   LogMainChannel       : Logging.Channel_ClassAccess;
   LogImplementation    : Logging.Implementation_Type;

   procedure OnCanSend
     (Item : in out ServerChannelCallBack_Type) is

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      loop
         PrevPosition := Item.Channel.WritePosition;
         case Item.SendStatus is
            when SendStatusReceive =>
               return;
            when SendStatusIdentify =>
               SimCommon.NetworkIDString'Write
                 (Item.Channel,
                  SimCommon.NetworkControlServerID);
               Item.SendStatus    := SendStatusReady;
               Item.ReceiveStatus := ReceiveStatusReady;
               Put("Identify as ContentServer");
               New_Line;
            when SendStatusReady =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.WritePosition:=PrevPosition;
   end OnCanSend;
   ---------------------------------------------------------------------------

   procedure OnReceive
     (Item : in out ServerChannelCallBack_Type) is

      use type SimCommon.NetworkIDString;

      PrevPosition : Ada.Streams.Stream_Element_Offset;

   begin
      Item.LogChannel.Write
        (Level   =>  Logging.LevelCommonEvent,
         Message => "Receive");
      loop
         PrevPosition := Item.Channel.ReceivePosition;
         case Item.ReceiveStatus is
            when ReceiveStatusWaitForIdentification =>
               declare
                  Identification : SimCommon.NetworkIDString;
               begin
                  SimCommon.NetworkIDString'Read
                    (Item.Channel,
                     Identification);
                  if Identification/=SimCommon.NetworkControlClientID then
                     Put("Identification of incomming connection is incorrect");
                     New_Line;
                     Item.ReceiveStatus := ReceiveStatusInvalid;
                  else
                     Put("Identification accepted");
                     Item.ReceiveStatus := ReceiveStatusReady;
                     Item.SendStatus    := SendStatusIdentify;
                  end if;
               end;
            when ReceiveStatusReady =>
               return;
            when ReceiveStatusInvalid =>
               return;
         end case;
      end loop;
   exception
      when Network.Streams.StreamOverflow =>
         Item.Channel.ReceivePosition:= PrevPosition;
   end OnReceive;
   ---------------------------------------------------------------------------

   procedure OnDisconnect
     (Item : in out ServerChannelCallBack_Type) is
      pragma Warnings(Off,Item);
   begin
      LogMainChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Disconnected Client");
      -- TODO: Free all stuff associated, for example the
      --       callback object
   end OnDisconnect;
   ---------------------------------------------------------------------------

   procedure OnAccept
     (Item    : in out ServerCallBack_Type;
      Channel : Network.Streams.Channel_ClassAccess) is
      pragma Warnings(Off,Item);

      NewCallBack : ServerChannelCallBack_Access;

   begin
      NewCallBack:=new ServerChannelCallBack_Type;
      NewCallBack.ReceiveStatus := ReceiveStatusWaitForIdentification;
      NewCallBack.SendStatus    := SendStatusReceive;
      NewCallBack.Channel       := Channel;
      LogContext.NewChannel
        (ChannelName => ConcatElements(Channel.PeerAddress),
         Channel     => NewCallBack.LogChannel);
      Channel.CallBack:=Network.Streams.ChannelCallBack_ClassAccess(NewCallBack);
      LogMainChannel.Write
        (Level   => Logging.LevelEvent,
         Message => "Accepted Client");
      Put(Channel.PeerAddress);
   end OnAccept;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin

      LogImplementation
        :=Logging.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Logging"));
      LogContext
        :=LogImplementation.NewContext
          (Configuration => Configuration,
           ModuleName => To_Unbounded_String("Control.Control"));

      LogContext.NewChannel
          (ChannelName => To_Unbounded_String("Server"),
           Channel     => LogMainChannel);

      StreamImplementation
        :=Network.Streams.Implementations.Find
          (Configuration => Configuration,
           ModuleName    => To_Unbounded_String("Control.Network"));

      StreamImplementation.Initialize.all;

      Server
        :=StreamImplementation.NewServer
          (Config => Config.GetModuleMap
               (Item => Configuration,
                Name => To_Unbounded_String
                  ("Control.Server.Network")).all);

      Server.CallBack:=ServerCallBack'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      LogImplementation.FreeContext
        (Item => LogContext);

      StreamImplementation.FreeServer
        (Item => Server);

      StreamImplementation.Finalize.all;

   end Finalize;
   ---------------------------------------------------------------------------

end SimControl.ControlServer;
