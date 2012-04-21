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
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Processes is

   -- WARNING: Return value is assumed to be a c int, this is true for
   --          GNU/linux.
--   function fork return Interfaces.C.int;
--   pragma Import(C,fork,"fork");

--   function Execve
--     (FileName  : Interfaces.C.Strings.chars_ptr;
--      Arguments : access Interfaces.C.Strings.chars_ptr;
--      Envp      : access Interfaces.C.Strings.chars_ptr)
--      return Interfaces.C.int;
--   pragma Import(C,Execve,"execve");

   function System
     (command : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;
   pragma Import(C,System,"system");
   ---------------------------------------------------------------------------

   procedure Execute
     (ProgramName : Unbounded_String;
      Arguments   : Unbounded_String) is

      use type Interfaces.C.int;

      CArgument    : aliased Interfaces.C.Strings.chars_ptr;

      Result : Interfaces.C.int;

   begin
      Put("PROC:");
      Put(To_String(ProgramName & " " & Arguments));
      New_Line;
      CArgument := Interfaces.C.Strings.New_String
        (To_String(ProgramName & " " & Arguments));
      Result:=System(CArgument);
      if Result=-1 then
         raise FailedExecute;
      end if;
      Interfaces.C.Strings.Free(CArgument);
   end Execute;
   ---------------------------------------------------------------------------

end Processes;
