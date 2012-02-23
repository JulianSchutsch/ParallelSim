with GNAT.OS_Lib;
with GNAT.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Network.Process is

   procedure Spawn
     (Program      : String;
      Arguments    : SpawnCallBackAccess;
      ProcessCount : Positive) is

      use type GNAT.OS_Lib.Process_Id;

      ID   : GNAT.OS_Lib.Process_Id;
      Args : access GNAT.OS_Lib.Argument_List;

      SubArgs : StringVector.Vector;
      SubArgsLength  : Integer;

      StringLength : Integer;

   begin

      for i in 1..ProcessCount loop

         SubArgs       := Arguments(i);
         SubArgsLength := Integer(StringVector.Length(SubArgs));
         Args:=new GNAT.OS_Lib.Argument_List(1..SubArgsLength);
         for i in 1..SubArgsLength loop
            StringLength := Length(SubArgs.Element(Index=>i));
            Args(i)      := new String(1..StringLength);
            Args(i).all  := To_String(SubArgs.Element(Index=>i));
         end loop;

         ID:=GNAT.OS_Lib.Non_Blocking_Spawn
               (Program_Name => Program,
                Args         => Args.all);

         if ID=GNAT.OS_Lib.Invalid_Pid then
            raise FailedSpawn;
         end if;
      end loop;

   end Spawn;

end Network.Process;
