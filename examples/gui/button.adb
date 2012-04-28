pragma Ada_2005;

with GUI;
with GUI.Button;
with GUI.Themes;
with GUI.UseImplementations;
with YellowBlue;
with Config;
with Fonts.Freetype;
with Basics; use Basics;

procedure Button is

   GUIImplementation : GUI.Implementation_Type;
   Context           : GUI.Context_ClassAccess;
   Button            : GUI.Button.Button_ClassAccess;
   Theme             : GUI.Themes.Implementation_Type;
   Configuration     : Config.Config_Type;

begin

   GUI.UseImplementations.Register;
   YellowBlue.Register;
   Fonts.Freetype.Register;

   GUIImplementation := GUI.Implementations.FindAny;
   Theme             := GUI.Themes.Implementations.FindAny;

   Context:=GUIImplementation.NewContext
     (Configuration => Configuration,
      Node          => U(""));

   Button:=Theme.NewButton(Context.WindowArea);
   Button.SetBounds
     (Top     => 10,
      Left    => 10,
      Height  => 25,
      Width   => 100,
      Visible => True);

   Fonts.Freetype.Unregister;
   YellowBlue.UnRegister;
   GUI.UseImplementations.Unregister;

end Button;
