with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Processes; use Processes;

package body Network.Processes is

   procedure Spawn
     (Program       : String;
      Configuration : Config.Modules;
      Amount        : Positive) is

   begin

      for i in 1..Amount loop

         Execute
           (ProgramName => Program,
            Arguments   => "-Network.Processes.Spawn");

      end loop;

   end Spawn;

end Network.Processes;
