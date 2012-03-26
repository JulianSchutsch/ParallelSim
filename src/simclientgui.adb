pragma Ada_2005;
with GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with ProcessLoop;
with GUI.Window;
with GUI.Themes;
with Ada.Text_IO; use Ada.Text_IO;

package body SimClientGUI is

   GUIImplementation   : GUI.Implementation_Type;
   ThemeImplementation : GUI.Themes.Implementation_Type;
   GUIContext          : GUI.Context_ClassAccess:=null;
   Terminated          : Boolean;
   Window              : GUI.Window.Window_ClassAccess:=null;

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
