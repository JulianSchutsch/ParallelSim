pragma Ada_2005;
with GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with ProcessLoop;
with GUI.Window;
with GUI.Themes;
with GUI.Console;
with Ada.Text_IO; use Ada.Text_IO;
with BoundsCalc;
with Fonts;

package body SimClientGUI is

   GUIImplementation   : GUI.Implementation_Type;
   ThemeImplementation : GUI.Themes.Implementation_Type;
   GUIContext          : GUI.Context_ClassAccess:=null;
   Terminated          : Boolean;
   Window              : GUI.Window.Window_ClassAccess:=null;
   Console             : GUI.Console.Console_ClassAccess:=null;

   procedure OnCloseContext
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

   begin
      Terminated:=True;
   end OnCloseContext;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is

   begin

      Terminated:=False;

      GUIImplementation
        :=GUI.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("GUI"));

      GUIContext
        :=GUIImplementation.NewContext
          (Configuration => Configuration,
           Node          => To_Unbounded_String("GUI"));

      ThemeImplementation
        :=GUI.Themes.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("GUI"));

      GUIContext.OnClose:=OnCloseContext'Access;

      Put("Create Window(SIMCLIENTGUI)");
      New_Line;
      Window:=ThemeImplementation.NewWindow
        (Parent => GUIContext.WindowArea);
      Window.SetBounds
        (Top     => 10,
         Left    => 10,
         Height  => 100,
         Width   => 200,
         Visible => True);
      Window.SetAnchors
        (Top    => True,
         Left   => True,
         Right  => False,
         Bottom => False);

      declare
         Bounds : constant BoundsCalc.Bounds_Type:=Window.Client.GetBounds;
      begin
         Console:=ThemeImplementation.NewConsole
           (Parent => GUI.Object_ClassAccess(Window));
         Console.SetFont
           (Font => Fonts.Lookup
              (Name       => To_Unbounded_String("./Vera.ttf"),
               Size       => 9,
               Attributes => Fonts.NoAttributes));
         Console.SetBounds
           (Top     => 0,
            Left    => 0,
            Height  => Bounds.Height,
            Width   => Bounds.Width,
            Visible => True);
         Console.SetAnchors
           (Top    => True,
            Left   => True,
            Right  => True,
            Bottom => True);
      end;
      Console.WriteLine
        (String => To_Unbounded_String("Hallo"),
         Color  => 16#FFFFFF00#);
      Console.WriteLine
        (String => To_Unbounded_String("Wie gehts?"),
         Color  => 16#FF00FF00#);

      Put("End of Simclientgui init");
      New_Line;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      Window.Finalize;

      GUIImplementation.FreeContext
        (Context => GUIContext);

   end Finalize;
   ---------------------------------------------------------------------------

   function Process
     return Boolean is
   begin
      ProcessLoop.Process;
      return Terminated;
   end Process;
   ---------------------------------------------------------------------------

end SimClientGUI;
