pragma Ada_2005;

with Xlib;
with OpenGL; use OpenGL;
with GUI.OpenGL;
with Ada.Unchecked_Deallocation;
with ProcessLoop;
with Interfaces.C.Strings;
with glX;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body GUI.OpenGL.Native is

   type CIntArray is array(Natural range <>) of aliased Interfaces.C.int;
   pragma Convention(C,CIntArray);

   XVisualAttribs : CIntArray:=
     (GLX.GLX_RGBA,
      GLX.GLX_RED_SIZE,8,
      GLX.GLX_GREEN_SIZE,8,
      GLX.GLX_BLUE_SIZE,8,
      GLX.GLX_DOUBLEBUFFER,
      GLX.GLX_DEPTH_SIZE,1,
      0);

   type Context_Type;
   type Context_Access is access all Context_Type;
   type Context_Type is new GUI.OpenGL.Context_Type with
      record
         Display             : XLib.Display_Access:=Null;
         NextContext         : Context_Access:=Null;
         LastContext         : Context_Access:=Null;
         DestroyedSignalSend : Boolean:=True;
         GLXMajor            : aliased GLint_Type;
         GLXMinor            : aliased GLint_Type;
         Screen              : Interfaces.C.int;
         Visual              : XLib.XVisualInfo_Access;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type,
      Name   => Context_Access);
   ---------------------------------------------------------------------------

   Contexts : Context_Access:=null;

   procedure Process is
      Context     : Context_Access;
      NextContext : Context_Access;
   begin
      Context:=Contexts;
      ContextLoop:
      while Context/=null loop

         NextContext:=Context.NextContext;
         -- DO SOMETHING FANCY WITH THE CONTEXT
         if Context.DestroyedSignalSend
           and Context.OnClose/=null then
            Context.OnClose(Context.CallBackObject);
         end if;

         Context:=NextContext;

      end loop ContextLoop;

   end Process;
   ---------------------------------------------------------------------------

   procedure FreeContext
     (Context : Context_ClassAccess) is

      Cont : Context_Access;

   begin
      Cont:=Context_Access(Context);
      -- Remove Context from the contexts list
      if Cont.LastContext/=null then
         Cont.LastContext.NextContext:=Cont.NextContext;
      else
         Contexts:=Cont.NextContext;
         if Contexts=null then
            ProcessLoop.Remove(Process'Access);
         end if;
      end if;

      if Cont.NextContext/=null then
         Cont.NextContext.LastContext:=Cont.LastContext;
      end if;

      Free(Cont);

   end FreeContext;
   ---------------------------------------------------------------------------

   function NewContext
     (Configuration : Config.Config_Type;
      Node          : Unbounded_String)
      return Context_ClassAccess is

      use type Xlib.Display_Access;
      use type XLib.XVisualInfo_Access;
      use type Interfaces.C.int;

      pragma Unreferenced(Configuration);
      pragma Unreferenced(Node);

      Context : Context_Access;

   begin
      -- Create new context object and add it to the contexts list
      Context:=new Context_Type;
      context.NextContext:=Contexts;
      if Contexts/=null then
         Contexts.LastContext:=Context;
      else
         ProcessLoop.Add(Process'Access);
      end if;

      Contexts:=Context;

      -- Initalize X context
      Context.Display:=Xlib.XOpenDisplay(Interfaces.C.Strings.Null_Ptr);
      if Context.Display=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to XOpenDisplay";
      end if;

      if glX.glXQueryVersion
        (dpy   => Context.Display,
         major => Context.GLXMajor'Access,
         minor => Context.GLXMinor'Access)=0 then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to glXQueryVersion";

      end if;
      Put("GLX Version:");
      Put(Integer(Context.GLXMajor));
      Put(".");
      Put(Integer(Context.GLXMinor));
      New_Line;
      if not
        (
           ((Context.GLXMajor=1) and (Context.GLXMinor>=2))
         or (Context.GLXMajor>1)) then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "GLX version is too small. Found "
             &GLint_Type'Image(Context.GLXMajor)&"."
             &GLint_Type'Image(Context.GLXMinor)
             &", but needed 1.2";

      end if;

      Context.Screen:=Xlib.DefaultScreen(Context.Display);

      Context.Visual:=GLX.glXChooseVisual
        (dpy        => Context.Display,
         screen     => Context.Screen,
         attribList => XVisualAttribs(0)'Access);
      if Context.Visual=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to glXChooseVisual";
      end if;


      Put("OK...SO FAR");
      New_Line;

      return Context_ClassAccess(Context);
   end NewContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access,
      FreeContext => FreeContext'Access);

   procedure Register is
   begin
      Implementations.Register
        (Identifier     => To_Unbounded_String("OpenGL"),
         Implementation => Implementation);
   end Register;
   ---------------------------------------------------------------------------

end GUI.OpenGL.Native;
