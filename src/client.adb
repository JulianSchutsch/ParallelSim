pragma Ada_2005;

with Config;
with GUI;
with GUI.OpenGL;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SimClientGUI;
with GUI.Themes.YellowBlue;

procedure Client is

   Configuration  : Config.Config_Type;

begin
   GUI.OpenGL.Register;
   GUI.Themes.YellowBlue.Register;

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("GUI.GUIImplementation"),
      New_Item  => To_Unbounded_String("OpenGL"));

   Config.Insert
     (Container => Configuration,
      Key       => To_Unbounded_String("GUI.Theme"),
      New_Item  => To_Unbounded_String("YellowBlue"));

   SimClientGUI.Initialize
     (Configuration => Configuration);

   loop
      exit when SimClientGUI.Process;
   end loop;

   SimClientGUI.Finalize;

end Client;
