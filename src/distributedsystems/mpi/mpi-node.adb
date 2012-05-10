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

with MPI; use MPI;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;
with Interfaces.C;
with Config;
with DistributedSystems; use DistributedSystems;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Regpat;
with Ada.Directories; use Ada.Directories;

package body MPI.Node is

   type Spawn_Type is new DistributedSystems.Spawn_Type with
      record
         Configuration       : Config.Config_Type;
         ExecutableArguments : Unbounded_String;
      end record;
   type Spawn_Access is access all Spawn_Type;

   overriding
   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : in out Config.Config_Type;
      Success          : out Boolean);
   ---------------------------------------------------------------------------

   procedure Execute
     (Item             : access Spawn_Type;
      SupplementConfig : in out Config.Config_Type;
      Success          : out Boolean) is

      use type StringStringMap_Pack.Cursor;

      Arguments      : Unbounded_String;
      ArgumentList   : GNAT.OS_Lib.Argument_List_Access;
      ExecutablePath : GNAT.OS_Lib.String_Access;
      TempFileName   : GNAT.OS_Lib.String_Access;
      TempFileDesc   : GNAT.OS_Lib.File_Descriptor;
      TempFile       : File_Type;
      ReturnCode     : Integer;

      ErrorLogin     : Boolean:=False;
      Failed         : Boolean:=False;

      Cursor         : StringStringMap_Pack.Cursor;

   begin

      Item.Configuration.SaveToFile("_MyConfig");

      Arguments := U("-noprompt ");

      Cursor:=SupplementConfig.Find(U("Account"));
      if Cursor/=StringStringMap_Pack.No_Element then
         declare
            Account  : constant Unbounded_String:=StringStringMap_Pack.Element(Cursor);
            Password : Unbounded_String;
            File     : File_Type;
         begin
            Cursor:=SupplementConfig.Find(U("Password"));
            if Cursor/=StringStringMap_Pack.No_Element then
               Password:=StringStringMap_Pack.Element(Cursor);
               begin
                  Delete_File("MPIEXEC.PWD");
               exception
                  when others =>
                     null;
               end;
               Create
                 (File => File,
                  Mode => Out_File,
                  Name => "MPIEXEC.PWD");
               Put_Line(File,To_String(Account));
               Put_Line(File,To_String(Password));
               Close(File);
               Arguments:=Arguments&"-pwdfile MPIEXEC.PWD ";
            end if;
         end;
      end if;

      Arguments := Arguments&Item.ExecutableArguments;

      ArgumentList := GNAT.OS_Lib.Argument_String_To_List(To_String(Arguments));
      GNAT.OS_Lib.Normalize_Arguments(ArgumentList.all);
      ExecutablePath:=GNAT.OS_Lib.Locate_Exec_On_Path("mpiexec");

      if Item.OnMessage/=null then
         Item.OnMessage("Running mpiexec with "&Arguments);
      end if;
      GNAT.OS_Lib.Create_Temp_Output_File
        (FD   => TempFileDesc,
         Name => TempFileName);
      GNAT.OS_Lib.Spawn
        (Program_Name           => ExecutablePath.all,
         Args                   => ArgumentList.all,
         Output_File_Descriptor => TempFileDesc,
         Return_Code            => ReturnCode,
         Err_To_Out             => True);
      GNAT.OS_Lib.Close(TempFileDesc);

      Open
        (File => TempFile,
         Mode => In_File,
         Name => TempFileName.all);

      if Item.OnMessage/=null then
         Item.OnMessage(U("Output from mpiexec:"));
      end if;
      while not End_Of_File(TempFile) loop
         declare
            Msg : constant String:=Get_Line(TempFile);
         begin
            if Item.OnMessage/=null then
               Item.OnMessage(U("* "&Msg));
            end if;
            if GNAT.Regpat.Match
              (Expression => "Credentials required to connect",
               Data       => Msg) then
               ErrorLogin := True;
               Failed     := True;
            end if;
            if GNAT.Regpat.Match
              (Expression => "Aborting",
               Data       => Msg) then
               Failed := True;
            end if;
         end;
      end loop;
      if Item.OnMessage/=null then
         Item.OnMessage(U("End of output, Return Code:"&Integer'Image(ReturnCode)));
      end if;
      Close(TempFile);
      GNAT.Strings.Free(ArgumentList);
      GNAT.Strings.Free(ExecutablePath);

      if ErrorLogin then
         Put_Line("Login ?");
         SupplementConfig.Include
           (Key      => U("Account"),
            New_Item => U(""));
         SupplementConfig.Include
           (Key      => U("Password"),
            New_Item => U(""));
      end if;

      Success:=not Failed;

   end Execute;
   ---------------------------------------------------------------------------

   procedure CreateSpawnObject
     (Configuration : Config.Config_Type;
      Executables   : ExecutableArray_Type;
      SpawnObject   : out Spawn_ClassAccess) is

      Spawn : Spawn_Access;

   begin

      Spawn:=new Spawn_Type;
      Spawn.Configuration:=Configuration;

      for i in Executables'Range loop
         Spawn.ExecutableArguments:=Spawn.ExecutableArguments& "-n"
           &Positive'Image(Executables(i).Amount)&" "
           &Executables(i).Executable
           &" -DistributedSystemsImplementation=MPI";
         if i/=Executables'Last then
            Spawn.ExecutableArguments:=Spawn.ExecutableArguments&" : ";
         end if;

      end loop;
      SpawnObject:=Spawn_ClassAccess(Spawn);

   end CreateSpawnObject;
   ---------------------------------------------------------------------------

   procedure InitializeNode
     (Configuration : out Config.Config_Type;
      Group         : DistributedSystems.Group_Type) is
      pragma Unreferenced(Group);

      use type Interfaces.C.int;

      WorldRank : aliased Interfaces.C.int;
      WorldSize : aliased Interfaces.C.int;
      Result    : Interfaces.C.int;

   begin

      Configuration.LoadFromFile
        (FileName => "_MyConfig");
      MPI_Init
        (Argc => null,
         Args => null);

      Result:=MPI_Comm_Rank
        (comm => MPI_COMM_WORLD,
         rank => WorldRank'Access);

      if Result/=MPI_SUCCESS then
         raise FailedNodeInitialization
           with "Call to MPI_Comm_Rank failed with "
             &ReturnValueToString(Result);
      end if;

      Result:=MPI_Comm_Size
        (comm => MPI_COMM_WORLD,
         size => WorldSize'Access);

      if Result/=MPI_Success then
         raise FailedNodeInitialization
           with "Call to MPI_COMM_Size failed with "
             &ReturnValueToString(Result);
      end if;

      MyGlobalID    := Node_Type(WorldRank);
      FirstGlobalID := 0;
      LastGlobalID  := Node_Type(WorldSize-1);

   end InitializeNode;
   ---------------------------------------------------------------------------

   procedure FinalizeNode is
   begin
      MPI_Finalize;
   end FinalizeNode;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (InitializeNode    => InitializeNode'Access,
      FinalizeNode      => FinalizeNode'Access,
      CreateSpawnObject => CreateSpawnObject'Access);

   procedure Register is
   begin

      Implementations.Register
        (Implementation => Implementation,
         Identifier     => U("MPI"));

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister
        (Identifier => U("MPI"));

   end Unregister;
   ---------------------------------------------------------------------------

end MPI.Node;
