with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Processes; use Processes;

package body Network.Processes is

   procedure Spawn
     (Program       : String;
      Amount        : Positive) is

   begin

      for i in 1..Amount loop

         Execute
           (ProgramName => Program,
            Arguments   => "-Network.Processes.Spawn");

      end loop;

   end Spawn;
   ---------------------------------------------------------------------------

   procedure StoreConfig
     (Configuration : Config.Config_Type) is
   begin
      null;
   end StoreConfig;
   ---------------------------------------------------------------------------

   procedure LoadConfig
     (Configuration : in out Config.Config_Type) is
   begin
      null;
   end LoadConfig;
   ---------------------------------------------------------------------------

end Network.Processes;
