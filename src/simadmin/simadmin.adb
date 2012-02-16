with Ada.Text_IO; use Ada.Text_IO;

package body SimAdmin is

   NetImplementation : Network.Config.Implementation;
   NetConfig         : StringStringMap.Map;

   procedure Initialize
     (NetworkImplementation : Network.Config.Implementation;
      Config                : StringStringMap.Map) is
   begin
      NetImplementation := NetworkImplementation;
      NetConfig         := Config;
   end Initialize;

   procedure Finalize is
   begin
      null;
   end Finalize;

   function Process
     return Boolean is

      Char : Character;
   begin
      Get(Char);
      return false;
   end Process;


end SimAdmin;
