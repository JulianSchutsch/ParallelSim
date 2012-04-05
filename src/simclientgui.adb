pragma Ada_2005;
with GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with ProcessLoop;
with GUI.Window;
with GUI.Themes;
with Ada.Text_IO; use Ada.Text_IO;
With Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with GUI.ScrollBar;
with BoundsCalc;

package body SimClientGUI is

   GUIImplementation   : GUI.Implementation_Type;
   ThemeImplementation : GUI.Themes.Implementation_Type;
   GUIContext          : GUI.Context_ClassAccess:=null;
   Terminated          : Boolean;
   Window              : GUI.Window.Window_ClassAccess:=null;
   ScrollBar           : GUI.ScrollBar.ScrollBar_ClassAccess:=null;

   procedure OnCloseContext
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

   begin
      Terminated:=True;
   end OnCloseContext;
   ---------------------------------------------------------------------------

   procedure OnPositionChange
     (CallBackObject : AnyObject_ClassAccess) is
      pragma Unreferenced(CallBackObject);
   begin
      Put("PositionChange called:");
      Put(ScrollBar.GetPosition);
      New_Line;
   end OnPositionChange;

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
         ScrollBar:=ThemeImplementation.NewVerticalScrollbar
           (Parent => GUI.Object_ClassAccess(Window));
         ScrollBar.OnPositionChange:=OnPositionChange'Access;
         ScrollBar.SetBounds
           (Top     => 0,
            Left    => Bounds.Width-ThemeImplementation.VerticalScrollbarWidth,
            Height  => Bounds.Height,
            Width   => ThemeImplementation.VerticalScrollBarWidth,
            Visible => True);
         ScrollBar.SetAnchors
           (Top    => True,
            Left   => False,
            Right  => True,
            Bottom => True);
      end;

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
