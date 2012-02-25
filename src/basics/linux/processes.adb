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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Processes is

   -- WARNING: Return value is assumed to be a c int, this is true for
   --          GNU/linux.
   function fork return Interfaces.C.int;
   pragma Import(C,fork,"fork");

   function Execve
     (FileName  : Interfaces.C.Strings.chars_ptr;
      Arguments : access Interfaces.C.Strings.chars_ptr;
      Envp      : access Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.int;
   pragma Import(C,Execve,"execve");

   procedure Execute
     (ProgramName : String;
      Arguments   : String) is

      use type Interfaces.C.int;

      CProgramName : Interfaces.C.Strings.chars_ptr;
      CArgument    : aliased array (0..2) of aliased Interfaces.C.Strings.chars_ptr;
      CEnvp        : aliased array (0..0) of aliased Interfaces.C.Strings.chars_ptr;

      Result : Interfaces.C.int;

   begin
      CProgramName := Interfaces.C.Strings.New_String(ProgramName);
      CArgument(0) := CProgramName;
      CArgument(1) := Interfaces.C.Strings.New_String(Arguments);
      CArgument(2) := Interfaces.C.Strings.Null_Ptr;
      CEnvp(0)     := Interfaces.C.Strings.Null_Ptr;
      Put(ProgramName);
      if fork=0 then
         Result:=Execve
           (FileName  => CProgramName,
            Arguments => CArgument(0)'Access,
            Envp      => CEnvp(0)'Access);
         Put("Returned with some error");
         Put(Integer(Result));
      end if;
   end Execute;

end Processes;
