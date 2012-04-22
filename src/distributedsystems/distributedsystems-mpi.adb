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

with Processes;
with MPI; use MPI;
with Ada.Text_IO; use Ada.Text_IO;
with Basics; use Basics;
with Interfaces.C;

package body DistributedSystems.MPI is

   procedure SpawnNodes
     (Configuration : Config.Config_Type;
      Executables   : ExecutableArray_Type) is

      Arguments : Unbounded_String;

   begin

      Configuration.SaveToFile
        (FileName => "_MyConfig");
      for i in Executables'Range loop
         Arguments:=Arguments& "-n"
           &Positive'Image(Executables(i).Amount)&" "
           &Executables(i).Executable
           &" -DistributedSystemsImplementation=MPI";
         if i/=Executables'Last then
            Arguments:=Arguments&" : ";
         end if;

      end loop;
      Put("Start mpiexec with ");
      Put(To_String(Arguments));
      New_Line;
      Processes.Execute
        (ProgramName => To_Unbounded_String("mpiexec"),
         Arguments   => Arguments);

   end SpawnNodes;
   ---------------------------------------------------------------------------

   procedure InitializeNode
     (Configuration : out Config.Config_Type;
      Group         : Group_Type) is
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
     (InitializeNode => InitializeNode'Access,
      FinalizeNode   => FinalizeNode'Access,
      SpawnNodes     => SpawnNodes'Access);

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

end DistributedSystems.MPI;
