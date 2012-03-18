pragma Ada_2005;

with Config;

package SimClientGUI is

   procedure Initialize
     (Configuration : Config.Config_Type);

   procedure Finalize;

   function Process
     return Boolean;

end SimClientGUI;
