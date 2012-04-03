pragma Ada_2005;
with GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with ProcessLoop;
with GUI.Window;
with GUI.TextView;
with GUI.Themes;
with Ada.Text_IO; use Ada.Text_IO;
with Fonts;
with BoundsCalc;

package body SimClientGUI is

   GUIImplementation   : GUI.Implementation_Type;
   ThemeImplementation : GUI.Themes.Implementation_Type;
   GUIContext          : GUI.Context_ClassAccess:=null;
   Terminated          : Boolean;
   Window              : GUI.Window.Window_ClassAccess:=null;
   Console             : GUI.TextView.TextView_ClassAccess:=null;

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
      GUI.SetBounds
        (Object => GUI.Object_ClassAccess(Window),
         Bounds =>
           (Top     => 10,
            Left    => 10,
            Height  => 100,
            Width   => 200,
            Visible => True));
      GUI.SetAnchors
        (Object => GUI.Object_ClassAccess(Window),
         Top    => True,
         Left   => True,
         Right  => False,
         Bottom => False);

      Console:=new GUI.TextView.TextView_Type;
      GUI.TextView.Initialize
        (Item   => GUI.TextView.TextView_Access(Console),
         Parent => GUI.Object_ClassAccess(Window));
      GUI.TextView.SetFont
        (Item => GUI.TextView.TextView_Access(Console),
         Font => Fonts.Lookup(To_Unbounded_String("./Vera.ttf"),19,Fonts.NoAttributes));
      declare
         Bounds : constant BoundsCalc.Bounds_Type:=GUI.GetBounds(Window.Client.all);
      begin
         GUI.SetBounds
           (Object => GUI.Object_ClassAccess(Console),
            Bounds =>
              (Top     => 0,
               Left    => 0,
               Height  => Bounds.Height,
               Width   => Bounds.Width,
               Visible => True));
      end;
      GUI.SetAnchors
        (Object => GUI.Object_ClassAccess(Console),
         Top    => True,
         Left   => True,
         Right  => True,
         Bottom => True);

      for i in 1..99 loop
         Console.WriteLine
           (String => To_Unbounded_String("A long test string to demonstrate wrapping, line "&Integer'Image(i)),
            Color  => 16#FF00FF00#);
      end loop;

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
