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

with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Directories; use Ada.Directories;
with Tools;

package body Plattform is

   procedure Detect is
   begin

      -- uname plattform detection, if possible
      Tools.QuickExec("uname","-s");
      if Tools.Success then
         if Tools.StringInOutput("MINGW") then
            Detected:=PlattformWindowsNT;
            ExecutableSuffix:=U(".exe");
            return;
         end if;
         if Tools.StringInOutput("Linux") then
            Detected:=PlattformLinux;
            return;
         end if;
         if Tools.StringInOutput("FreeBSD") then
            Detected:=PlattformBSD;
            return;
         end if;
      end if;

      if Tools.StringInEnvironment("Windows_NT","OS") then
         Detected:=PlattformWindowsNT;
         return;
      end if;

      Detected:=PlattformUnknown;

   end Detect;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      Detect;
      case Detected is
         when PlattformLinux | PlattformBSD =>
            Tools.QuickExec
              (Command => "gcc",
               Argument => "unixconstants.c"&
               " -o unixconstants"&To_String(ExecutableSuffix));
            if not Tools.Success then
               raise PlattformInitializationFailed with
                 "Failed to compile unixconstants.c";
            end if;
            Tools.QuickExec
              (Command  => "./unixconstants",
               Argument => "");
            if not Tools.Success then
               raise PlattformInitializationFailed with
                 "Failed to run unixconstants";
            end if;
            Copy_File
              (Source_Name => "unixconstants.ads",
               Target_Name => To_String(BasePath)&"../src/unix/unixconstants.ads");
            Delete_File("unixconstants.ads");
         when others =>
            null;
      end case;
   end Initialize;
   ---------------------------------------------------------------------------

end Plattform;
