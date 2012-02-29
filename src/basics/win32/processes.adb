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
with System;
pragma Warnings(OFF);
with System.Win32;
pragma Warnings(ON);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Processes is

   type SecurityAttributes is
      record
         nLength              : Interfaces.Unsigned_32;
         lpSecurityDescriptor : System.Address;
         bInheritedHandle     : Boolean;
      end record;
   pragma Convention(C,SecurityAttributes);

   type StartupInfo is
      record
         cb : Interfaces.Unsigned_32;
         lpReserved      : Interfaces.C.Strings.chars_ptr:=Interfaces.C.Strings.Null_Ptr;
         lpDesktop       : Interfaces.C.Strings.chars_ptr:=Interfaces.C.Strings.Null_Ptr;
         lpTitle         : Interfaces.C.Strings.chars_ptr:=Interfaces.C.Strings.Null_Ptr;
         dwX             : Interfaces.Unsigned_32:=0;
         dwY             : Interfaces.Unsigned_32:=0;
         dwXSize         : Interfaces.Unsigned_32:=0;
         dwYSize         : Interfaces.Unsigned_32:=0;
         dwXCountChars   : Interfaces.Unsigned_32:=0;
         dwYCountChars   : Interfaces.Unsigned_32:=0;
         dwFillAttribute : Interfaces.Unsigned_32:=0;
         dwFlags         : Interfaces.Unsigned_32:=0;
         wShowWindow     : Interfaces.Unsigned_16:=0;
         cbReserved2     : Interfaces.Unsigned_16:=0;
         hStdInput       : Interfaces.C.ptrdiff_t:=0;
         hStdOutput      : Interfaces.C.ptrdiff_t:=0;
         hStderr         : Interfaces.C.ptrdiff_t:=0;
      end record;
   pragma Convention(C,StartupInfo);

   type ProcessInformation is
      record
         hProcess    : Interfaces.C.ptrdiff_t:=0;
         hThread     : Interfaces.C.ptrdiff_t:=0;
         dwProcessID : Interfaces.Unsigned_32:=0;
         dwThreadID  : Interfaces.Unsigned_32:=0;
      end record;
   pragma Convention(C,ProcessInformation);

   function CreateProcess
     (lpApplicationName    : Interfaces.C.Strings.chars_ptr;
      lpCommandLine        : Interfaces.C.Strings.chars_ptr;
      lpProcessAttributes  : access SecurityAttributes:=null;
      lpThreadAttributes   : access SecurityAttributes:=null;
      bInheritHandles      : Interfaces.C.int;
      dwCreationFlags      : Interfaces.Unsigned_32;
      lpEnvironment        : System.Address;
      lpCurrentDirectory   : Interfaces.C.Strings.chars_ptr;
      lpStartupInfo        : access StartupInfo:=null;
      lpProcessInformation : access ProcessInformation)
      return Interfaces.C.int;
   pragma Import(StdCall,CreateProcess,"CreateProcessA");

   function CloseHandle
     (hObject : Interfaces.C.ptrdiff_t)
      return Interfaces.C.int;
   pragma Import(Stdcall,CloseHandle,"CloseHandle");

   procedure Execute
     (ProgramName : String;
      Arguments   : String) is

      use type Interfaces.C.int;
      use type Interfaces.Unsigned_32;

      CProgramName : Interfaces.C.Strings.chars_ptr;
      CCommandLine : Interfaces.C.Strings.chars_ptr;
      ProcessInfo  : aliased ProcessInformation;
      Startup : aliased StartupInfo;
      Result : Interfaces.C.int;

   begin
      CProgramName := Interfaces.C.Strings.New_String(ProgramName);
      CCommandLine := Interfaces.C.Strings.New_String(ProgramName & " " & Arguments);

      Startup.cb := Startup'Size/8;

      if CreateProcess
        (lpApplicationName    => CProgramName,
         lpCommandLine        => CCommandLine,
         bInheritHandles      => 0,
         dwCreationFlags      => 16#10#,
         lpEnvironment        => System.Null_Address,
         lpCurrentDirectory   => Interfaces.C.Strings.Null_Ptr,
         lpStartupInfo        => Startup'Access,
         lpProcessInformation => ProcessInfo'Access)=0 then
         Put("Failed");
         Put(Integer(System.Win32.GetLastError));
      end if;

      Result := CloseHandle(ProcessInfo.hProcess);
      if Result=0 then
         raise FailedExecute with "CloseHandle for hProcess failed";
      end if;
      Result := CloseHandle(ProcessInfo.hThread);
      if Result=0 then
         raise FailedExecute with "CloseHandle for hThread failed";
      end if;

      Interfaces.C.Strings.Free(CProgramName);
      Interfaces.C.Strings.Free(CCommandLine);
   end Execute;

begin
   Suffix:=To_Unbounded_String(".exe");
end Processes;
