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
with MPI;
with Ada.Text_IO; use Ada.Text_IO;

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
   begin

      Configuration.LoadFromFile
        (FileName => "_MyConfig");
      Standard.MPI.Init
        (Argc => null,
         Args => null);

   end InitializeNode;
   ---------------------------------------------------------------------------

   procedure FinalizeNode is
   begin
      Standard.MPI.Finalize;
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
         Identifier     => To_Unbounded_String("MPI"));

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister
        (Identifier => To_Unbounded_String("MPI"));

   end Unregister;
   ---------------------------------------------------------------------------

end DistributedSystems.MPI;
