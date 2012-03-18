pragma Ada_2005;

with Config;
with GUI;
with GUI.OpenGL;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SimClientGUI;

procedure Client is

   Configuration  : Config.Config_Type;

begin
   GUI.OpenGL.Register;

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("GUI.GUIImplementation"),
      New_Item  => To_Unbounded_String("OpenGL"));

   SimClientGUI.Initialize
     (Configuration => Configuration);

   loop
      exit when SimClientGUI.Process;
   end loop;

   SimClientGUI.Finalize;

end Client;
