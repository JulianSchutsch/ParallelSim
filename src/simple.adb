with Basics; use Basics;
with ProgramArguments;
with Network.Config;
with Network.Streams;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Simple is

   function Test
     (Str : Unbounded_String)
      return Network.Config.Implementation_Type is

      Impl : Network.Config.Implementation_Type;

   begin
      Put(Str);
      return Impl;
   end Test;

   Cursor : StringStringMap.Cursor;
   Res : Network.Config.Implementation_Type;
   use type Network.Streams.InitializeAccess;

begin
   ProgramArguments.Initialize;

   Cursor:=StringStringMap.Find
     (Container => ProgramArguments.VariablesMap,
      Key => To_Unbounded_String("test"));

   Res:=Test(StringStringMap.Element(Cursor));

   if Res.Streams.Initialize/=null then
      Put("Miracle");
   end if;
end Simple;
