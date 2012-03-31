pragma Ada_2005;

with Config;
with GUI;
with GUI.OpenGL;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SimClientGUI;
with GUI.Themes.YellowBlue;
with Fonts;
with Fonts.Freetype;
with ExceptionOutput;

procedure Client is

   Configuration  : Config.Config_Type;
   MyFont : Fonts.Font_ClassAccess;
   pragma Unreferenced(MyFont);

begin
   GUI.OpenGL.Register;
   GUI.Themes.YellowBlue.Register;
   Fonts.Freetype.Initialize;

   MyFont:=Fonts.Lookup
     (Name       => To_Unbounded_String("./Vera.ttf"),
      Size       => 16,
      Attributes => Fonts.NoAttributes);

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

   Fonts.Freetype.Finalize;
exception
   when E:others =>
      ExceptionOutput.Put(E);

end Client;
