pragma Ada_2005;
with GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Basics; use Basics;
with ProcessLoop;

package body SimClientGUI is

   GUIImplementation : GUI.Implementation_Type;
   GUIContext        : GUI.Context_ClassAccess:=null;
   Terminated        : Boolean;

   procedure OnCloseContext
     (CallBackObject : AnyObject_ClassAccess) is

      pragma Unreferenced(CallBackObject);

   begin
      Terminated:=True;
   end OnCloseContext;
   ---------------------------------------------------------------------------

   procedure Initialize
     (Configuration : Config.Config_Type) is

      ConfigMap : StringStringMap.Map;

   begin

      Terminated:=False;

      GUIImplementation
        :=GUI.Implementations.Find
          (Configuration => Configuration,
           Node          => To_Unbounded_String("GUI"));

      GUIContext
        :=GUIImplementation.NewContext
          (Configuration => ConfigMap);

      GUIContext.OnClose:=OnCloseContext'Access;

   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      GUIImplementation.FreeContext
        (Context => GUIContext);
   end Finalize;

   function Process
     return Boolean is
   begin
      ProcessLoop.Process;
      return Terminated;
   end Process;
   ---------------------------------------------------------------------------

end SimClientGUI;
