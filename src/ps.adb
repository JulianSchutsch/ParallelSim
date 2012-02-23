pragma Ada_2005;

with Endianess;
with Types;
with Network;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with BSDSockets;
with Basics; use Basics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Network.Streams;
with Network.Barrier;
with Network.Config;
with BSDSockets.Streams;
with Network.Messages;
with Network.Process;

with SimControl;
with SimElement;
with SimAdmin;
with Config;

with ProgramArguments;

procedure Ps is
   use type StringStringMap.Cursor;

   NetworkImplementation: constant Network.Config.Implementation:=
     (NewStreamServer  => BSDSockets.Streams.NewStreamServer'Access,
      NewStreamClient  => BSDSockets.Streams.NewStreamClient'Access,
      FreeStreamServer => BSDSockets.Streams.FreeStreamServer'Access,
      FreeStreamClient => BSDSockets.Streams.FreeStreamClient'Access,
      SpawnProcesses   => Network.Process.Spawn'Access);

begin

   BSDSockets.Initialize;

   -- Check if this is a start or an instance program
   if ProgramArguments.VariablesMap.Find
     (Key=> To_Unbounded_String("instance"))
     /=StringStringMap.No_Element then
      -- This is an instance program, wait for instructions
      null;
   else
      -- This is a start program
      null;
   end if;
 --     BSDSockets.Process;
 --     BSDSockets.Streams.Process;

   BSDSockets.Finalize;
end Ps;
