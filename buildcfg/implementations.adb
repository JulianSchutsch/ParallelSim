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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib;
with GNAT.Strings;
with Plattform; use Plattform;
with Tools;

package body Implementations is

   -- MPICH2 must find the location of the libmpi and mpi.h first.
   -- On Windows this is done by finding the base path though
   -- locating mpiexec over the path environment variable.
   -- On Linux it is assumed that only a /mpich2 must be added for
   -- the mpi.h and the .so is found without any further knowledge.
   procedure MPICH2_Initialize is

      File : File_Type;
      MPIBasePath : Unbounded_String;
      MPICH2Lib : StringList_Pack.List;
      MPICH2Inc : StringList_Pack.List;

      function CleanFileName
        (Name : String)
         return String is
      begin
         if Name'Length=0 then
            return Name;
         end if;
         if Name(Name'First)='"' then
            return Name(Name'First+1..Name'Last-1);
         else
            return Name;
         end if;
      end CleanFileName;
      ------------------------------------------------------------------------

      procedure GetWindowsMPICH2BasePath is

         use type GNAT.Strings.String_Access;

         ExecPath : GNAT.OS_Lib.String_Access;

      begin
         ExecPath:=GNAT.OS_Lib.Locate_Exec_On_Path
           (Exec_Name => "mpiexec.exe");
         if ExecPath=null then
            raise ImplementationInitializeFailed
              with "Unable to locate mpiexec.exe";
         end if;
         MPIBasePath:=U(ExecPath.all(ExecPath.all'First..ExecPath.all'Last-16));
         GNAT.Strings.Free(ExecPath);
      end GetWindowsMPICH2BasePath;
      ------------------------------------------------------------------------

   begin

      if not Exists(To_String(BasePath)&"mpiconstants.c") then
         raise ImplementationInitializeFailed
           with "mpiconstant.c not found, maybe execution path is not in ./buildcfg";
      end if;

      Tools.DeleteFile(To_String(BasePath)&"mpiconstants.h");

      Create
        (File => File,
         Mode => Out_File,
         Name => To_String(BasePath)&"mpiconstants.h");

      case Detected is
         when PlattformLinux | PlattformBSD=>
            Put_Line(File,"#include ""mpi.h""");
            Tools.QuickExec
              (Command => "mpicc",
               Argument => "-compile-info");
            if Tools.Success then
               MPICH2Inc:=Tools.ExtractArgsFromOutput
                 (Args => (0=>U("-I")));
            end if;
            Tools.QuickExec
              (Command => "mpicc",
               Argument => "-link-info");
            if Tools.Success then
               MPICH2Lib:=Tools.ExtractArgsFromOutput
                 (Args => (U("-L"),U("-l")));
            end if;

         when PlattformWindowsNT =>
            GetWindowsMPICH2BasePath;
            Put("MPICH2 Base Path :"&To_String(MPIBasePath));
            New_Line;
            if not Exists(To_String(MPIBasePath&"/include/mpi.h")) then
               Close(File);
               raise ImplementationInitializeFailed
                 with "mpi.h not found. Please set MPIBASE environment variable.";
            end if;
            Put_Line(File,"#include """&To_String(MPIBasePath)&"/include/mpi.h""");
            MPICH2Lib.Append("-L"&MPIBasePath&"\lib");
            MPICH2Lib.Append(U("-lmpi"));

         when others =>
            Close(File);
            raise ImplementationInitializeFailed
              with "Uncertain where to look for mpi.h, please add to implementations.adb";

      end case;

      AdditionalConfigLines.Append
        ("  MPICH2Lib:="&Tools.StringListToGprList(MPICH2Lib)&";");
      AdditionalConfigLines.Append
        ("  MPICH2Inc:="&Tools.StringListToGprList(MPICH2Inc)&";");

      Close(File);

      Tools.QuickExec
        (Command  => "gcc",
         Argument => To_String(BasePath)&"mpiconstants.c -o "
         &To_String(BasePath)&"mpiconstants"&To_String(ExecutableSuffix)
         &" -I/usr/include/mpich2"
         &" -I/usr/include/mpich2-i386"
         &" -I/usr/local/include");

      if not Tools.Success then
         raise ImplementationInitializeFailed
           with "Unable to compile mpiconstants.c for mpich2 module";
      end if;

      Tools.QuickExec
        (Command  => "mpiconstants",
         Argument => "");

      Copy_File
        (Source_Name => "mpiconstants.ads",
         Target_Name => To_String(BasePath)&"../src/distributedsystems/mpi/mpiconstants.ads");
      Delete_File("mpiconstants.ads");

   end MPICH2_Initialize;
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin

      case Detected is
         when PlattformUnknown =>
            null;
         when PlattformLinux | PlattformBSD =>
            Default(ImplementationBSDSockets) := True;
            Default(ImplementationXlib)       := True;
            Default(ImplementationMPICH2)     := True;
            Default(ImplementationFreeType)   := True;
         when PlattformWindowsNT =>
            Default(ImplementationBSDSockets) := True;
            Default(ImplementationWGL)        := True;
            Default(ImplementationMPICH2)     := True;
            Default(ImplementationFreeType)   := True;
      end case;
      Default(ImplementationBitmapFonts) := True;

   end;
   ---------------------------------------------------------------------------

   procedure FindImplementation
     (Name           : Unbounded_String;
      Module         : out Module_Enum;
      Implementation : out Implementation_Enum) is
   begin

      for i in Implementations'Range loop

         if Implementations(i).Name=Name then
            Module         := Implementations(i).Module;
            Implementation := i;
            return;
         end if;

      end loop;

      raise ImplementationNotFound with To_String(Name);

   end FindImplementation;
   ---------------------------------------------------------------------------

end Implementations;
