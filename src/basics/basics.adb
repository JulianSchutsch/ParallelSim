--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.

pragma Ada_2005;

with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces;
with System.Address_Image;
with System.WCh_Cnv;
with System.WCh_Con;

package body Basics is
   use type Ada.Containers.Hash_Type;

   function UCS2ToUTF8
     (Char : Wide_Character)
      return Unbounded_String is

      procedure Add(PartChar : Character);
      procedure Convert is new System.WCh_Cnv.Wide_Char_To_Char_Sequence
        (Add);

      Result : Unbounded_String;

      procedure Add(PartChar : Character) is
      begin
         Result:=Result&PartChar;
      end Add;

   begin
      Convert(Char,System.WCh_Con.WCEM_UTF8);
      return Result;
   end UCS2ToUTF8;
   ---------------------------------------------------------------------------

   function UTF8ToUCS4
     (String : Unbounded_String)
      return Unbounded_Wide_Wide_String is

      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_32;

      function ToUnsigned8 is new Ada.Unchecked_Conversion
        (Source => Character,
         Target => Interfaces.Unsigned_8);

      function ToWideWideChar is new Ada.Unchecked_Conversion
        (Source => Interfaces.Unsigned_32,
         Target => Wide_Wide_Character);

      Buffer      : Wide_Wide_String(1..Length(String));
      BufferUsed  : Natural:=0;
      CurrentChar : Interfaces.Unsigned_8;
      Code        : Interfaces.Unsigned_32;
      StringPos   : Integer;
      ByteCount   : Integer;

   begin

      StringPos:=1;
      while StringPos<=Length(String) loop
         CurrentChar:=ToUnsigned8(Element(String,StringPos));
         Put("First Char:");
         Put(Interfaces.Unsigned_8'Image(CurrentChar));
         New_Line;
         case CurrentChar is
            when 0..16#80#-1 =>
               ByteCount := 0;
               Code      := Interfaces.Unsigned_32(CurrentChar);
            when 16#80#..16#E0#-1 =>
               ByteCount := 1;
               Code      := Interfaces.Unsigned_32(CurrentChar and 16#1F#);
            when 16#E0#..16#F0#-1 =>
               ByteCount := 2;
               Code      := Interfaces.Unsigned_32(CurrentChar and 16#0F#);
            when 16#F0#..16#F8#-1 =>
               ByteCount := 3;
               Code      := Interfaces.Unsigned_32(CurrentChar and 16#07#);
            when others =>
               raise UTF8Exception
                 with "UTF8 invalid first byte:"
                   &Interfaces.Unsigned_8'Image(CurrentChar);
         end case;
         Put("ByteCount");
         Put(Integer'Image(ByteCount));
         Put("/");
         Put(Interfaces.Unsigned_32'Image(Code));
         Put("::");
         StringPos:=StringPos+1;
         for b in 1..ByteCount loop
            if StringPos>Length(String) then
               raise UTF8Exception
                 with "UTF8 incomplete code";
            end if;
            Code := Interfaces.Shift_Left(Code,6)
              +Interfaces.Unsigned_32(ToUnsigned8(Element(String,StringPos)) and 16#3F#);
            Put("Ext");
            Put(Interfaces.Unsigned_8'Image(ToUnsigned8(Element(String,StringPos)) and 16#3F#));
            New_LIne;
            StringPos:=StringPos+1;
         end loop;

         Put("Code");
         Put(Interfaces.Unsigned_32'Image(Code));
         New_Line;
         BufferUsed         := BufferUsed+1;
         Buffer(BufferUsed) := ToWideWideChar(Code);

      end loop;
      return To_Unbounded_Wide_Wide_String(Buffer(1..BufferUsed));

   end UTF8ToUCS4;
   ---------------------------------------------------------------------------

   function StringSumHash
     (id: Unbounded_String)
      return Ada.Containers.Hash_Type is

      Sum : Ada.Containers.Hash_Type:=0;

   begin
      for i in 1..Length(id) loop
         Sum := Sum+Character'Pos(Element(id,i));
      end loop;
      return Sum;
   end;
   ---------------------------------------------------------------------------

   procedure Put
     (Item : StringStringMap.Map) is

      use type StringStringMap.Cursor;

      Cursor : StringStringMap.Cursor;

   begin
      Cursor:=Item.First;
      while Cursor/=StringStringMap.No_Element loop
         Put(StringStringMap.Key(Cursor));
         Put(" : ");
         Put(StringStringMap.Element(Cursor));
         New_Line;
         Cursor:=StringStringMap.Next(Cursor);
      end loop;
   end;
   ---------------------------------------------------------------------------

   function ConcatElements
     (Item      : StringStringMap.Map;
      Separator : Unbounded_String)
      return Unbounded_String is

      use type StringStringMap.Cursor;

      Cursor    : StringStringMap.Cursor;
      NewString : Unbounded_String;

   begin
      Cursor:=Item.First;
      while Cursor/=StringStringMap.No_Element loop
         if NewString/="" then
            NewString:=NewString & Separator;
         end if;
         NewString:=NewString & StringStringMap.Element(Cursor);
         Cursor:=StringStringMap.Next(Cursor);
      end loop;
      return NewString;
   end ConcatElements;
   ---------------------------------------------------------------------------

   function RoundUpPowerOf2
     (Value : Natural)
      return Natural is

      Result : Natural;

   begin

      if Value=0 then
         return 0;
      end if;

      Result:=1;

      while Result<Value loop
         Result:=Result*2;
      end loop;

      return Result;

   end RoundUpPowerOf2;
   ---------------------------------------------------------------------------

   procedure Put
     (Address : System.Address) is
   begin
      Put(System.Address_Image(Address));
   end Put;
   ---------------------------------------------------------------------------

   procedure Swap
     (Value1 : in out Float;
      Value2 : in out Float) is

      Temp : Float;

   begin
      Temp:=Value1;
      Value1:=Value2;
      Value2:=Temp;
   end Swap;
   ---------------------------------------------------------------------------

end Basics;
