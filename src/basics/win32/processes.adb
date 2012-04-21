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

with Interfaces.C;
with Interfaces.C.Strings;
with Win32;
with Win32.Shell32;

--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Processes is

   procedure Execute
     (ProgramName : Unbounded_String;
      Arguments   : Unbounded_String) is

      use type Interfaces.C.int;
      use type Interfaces.Unsigned_32;
      use type Win32.HINSTANCE_Type;

      CProgramName : Interfaces.C.Strings.chars_ptr;
      CParameters  : Interfaces.C.Strings.chars_ptr;
      HInstance : Win32.HINSTANCE_Type;

   begin
      CProgramName := Interfaces.C.Strings.New_String(To_String(ProgramName));
      CParameters  := Interfaces.C.Strings.New_String(To_String(Arguments));

      HInstance:=Win32.Shell32.ShellExecute
        (hwnd         => 0,
         lpOperation  => Interfaces.C.Strings.Null_Ptr,
         lpFile       => CProgramName,
         lpParameters => CParameters,
         lpDirectory  => Interfaces.C.Strings.Null_Ptr,
         nShowCmd     => Win32.SW_SHOWNOACTIVATE);

      Interfaces.C.Strings.Free(CProgramName);
      Interfaces.C.Strings.Free(CParameters);
      if HInstance<=32 then
         raise FailedExecute with "HInstance returned :"
           &Win32.HINSTANCE_Type'Image(HInstance);
      end if;

   end Execute;

end Processes;
