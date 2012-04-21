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

-- Revision History
--   18.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

package Win32.Kernel32 is

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

   function GetModuleHandle
     (lpModuleName : LPCTSTR_Type)
      return HInstance_Type;
   pragma Import(StdCall,GetModuleHandle,"GetModuleHandleA");

end Win32.Kernel32;
