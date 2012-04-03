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
with Ada.Unchecked_Conversion;

package body GUI.OpenGL.Native is

   ErrorStorage : Xlib.XErrorEvent_Type;
   ErrorStorageSet : Boolean;

   function ErrorCodeString
     return String is

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.char,
         Target => Interfaces.Unsigned_8);
   begin
      if ErrorStorageSet then
         return " Serial:"&Interfaces.C.long'Image(ErrorStorage.serial)
           &" Error Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.error_code))
           &" Request Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.request_code))
           &" Minor Code:"&Interfaces.Unsigned_8'Image(Convert(ErrorStorage.minor_code));
      else
         return " No further information since the error handler was not called!";
      end if;
   end;
   ---------------------------------------------------------------------------

   function ErrorHandler
     (display : Xlib.Display_Access;
      error   : XLib.XErrorEvent_Access)
      return Interfaces.C.int;
   pragma Convention(C,ErrorHandler);

   function ErrorHandler
     (display : Xlib.Display_Access;
      error   : XLib.XErrorEvent_Access)
      return Interfaces.C.int is

      pragma Unreferenced(display);

   begin
      ErrorStorage    := error.all;
      ErrorStorageSet := True;
      Put("**********************ErrorHandler Called*********************");
      Put(ErrorCodeString);
      New_Line;
      return 0;
   end ErrorHandler;

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
         Display             : XLib.Display_Access     := Null;
         NextContext         : Context_Access          := Null;
         LastContext         : Context_Access          := Null;
         DestroyedSignalSend : Boolean                 := False;
         GLXMajor            : aliased GLint_Type      := 0;
         GLXMinor            : aliased GLint_Type      := 0;
         Screen              : Interfaces.C.int        := 0;
         Visual              : XLib.XVisualInfo_Access := Null;
         ColorMap            : XLib.ColorMap_Type      := 0;
         Window              : XLib.Window_Type        := 0;
         DeleteWindowAtom    : aliased XLib.Atom_Type  := 0;
         GLXContext          : glX.GLXContext_Access   := Null;
         InputIM             : Xlib.XIM_Access         := Null;
         InputContext        : Xlib.XIC_Access         := Null;
         DoubleBuffered      : Boolean                 := True;
         ContextInitialized  : Boolean                 := False;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Context_Type,
      Name   => Context_Access);
   ---------------------------------------------------------------------------
   Contexts : Context_Access:=null;

   procedure Process
     (Context : Context_Access) is

      use type Interfaces.C.int;
      use type Interfaces.C.long;
      use type Xlib.Window_Type;

      EventCount : Interfaces.C.int;

      Event : aliased Xlib.XEvent_Type;

      procedure Paint is
      begin
               GUI.OpenGL.Paint(GUI.OpenGL.Context_Type(Context.all));
               if Context.DoubleBuffered then
                  glX.glXSwapBuffers
                    (dpy => Context.Display,
                     drawable => glX.GLXDrawable_Type(Context.Window));
               else
                  glFinish;
               end if;
      end Paint;

   begin
      if glX.glxMakeCurrent
        (dpy      => Context.Display,
         drawable => glX.GLXDrawable_Type(Context.Window),
         context  => Context.GLXContext)=0 then
         raise InvalidContext
           with "glxMakeCurrent failed";
      end if;

      EventCount:=Xlib.XPending
        (display => Context.Display);

      while EventCount>0 loop
         EventCount := EventCount-1;

         -- WARNING : Unchecked access necessary since Xlib makes use
         --           of the passed structure as temporary buffer,
         --           which is local to this Context.
         Xlib.XNextEvent
           (display => Context.Display,
            event_return => Event'Unchecked_Access);

         case Event.ttype is

            when Xlib.ClientMessage =>

               Put("ClientMessage");
               Put(Integer(event.ClientMessage.l(0)));
               Put(Integer(Integer(Context.DeleteWindowAtom)));
               New_Line;

               if Event.ClientMessage.l(0)
                 =Interfaces.C.long(Context.DeleteWindowAtom) then
                  Context.DestroyedSignalSend:=True;
                  return;
               end if;

            when Xlib.Expose =>
               Paint;

            when Xlib.ButtonPress =>
               -- FAULT : Please may some guru explain to me why i have an
               -- offset of two pixels in every window i create using xlib?
               case Event.ButtonPress.button is
                  when Xlib.Button1 =>
                     MouseDown
                       (Context     => Context_ClassAccess(Context),
                        MouseButton => LeftButton,
                        AbsX        => Integer(Event.ButtonPress.x),
                        AbsY        => Integer(Event.ButtonPress.y-2));
                  when Xlib.Button2 =>
                     null;
                  when others =>
                     Put("Unknown Button pressed");
                     Put(Integer(Event.ButtonPress.button));
                     New_Line;
               end case;

            when Xlib.ButtonRelease =>
               case Event.ButtonRelease.button is
                  when Xlib.Button1 =>
                     MouseUp
                       (Context     => Context_ClassAccess(Context),
                        MouseButton => LeftButton,
                        AbsX        => Integer(Event.ButtonPress.x),
                        AbsY        => Integer(Event.ButtonPress.y-2));
                  when Xlib.Button2 =>
                     null;
                  when others =>
                     null;
               end case;

            when Xlib.MotionNotify =>
               MouseMove
                 (Context => Context_ClassAccess(Context),
                  AbsX    => Integer(Event.ButtonMotion.x),
                  AbsY    => Integer(Event.ButtonMotion.y-2));

            when Xlib.KeyPress =>
               null;

            when Xlib.KeyRelease =>
               null;

            when Xlib.ConfigureNotify =>
               GUI.Resize
                 (Context => Context_ClassAccess(Context),
                  Height  => Integer(Event.Configure.height),
                  Width   => Integer(Event.Configure.width));
               -- TODO: Send update signal

            when Xlib.ResizeRequest =>

               Put("Resize");
               New_Line;
               Context.Bounds:=
                 (Top     => 0,
                  Left    => 0,
                  Height  => Integer(Event.ResizeRequest.height),
                  Width   => Integer(Event.ResizeRequest.width),
                  Visible => True);
               -- TODO: Send update signal

            when Xlib.ReparentNotify =>
               null;

            when others =>
               Put("Unknown Event Type:");
               Put(Integer(Event.ttype));
               New_Line;
         end case;

      end loop;

      Paint;

   end Process;
   ---------------------------------------------------------------------------

   procedure Process is
      Context     : Context_Access;
      NextContext : Context_Access;
   begin
      Context:=Contexts;
      ContextLoop:
      while Context/=null loop

         NextContext:=Context.NextContext;
         Process(Context);
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

      use type glX.GLXContext_Access;
      use type Interfaces.C.int;
      use type Xlib.Window_Type;
      use type Xlib.Display_Access;
      use type Xlib.XVisualInfo_Access;

      Cont : Context_Access;

   begin
      Cont:=Context_Access(Context);

      -- Deinitialize basic window environment
      if Cont.ContextInitialized then
         GUI.Finalize
           (Context => GUI.Context_Type(Cont.all));
      end if;

      if Cont.Visual/=null then
         Xlib.XFree
           (Cont.Visual.all'Address);
      end if;

      if Cont.GLXContext/=null then

         if glX.glXMakeCurrent
           (dpy      => Cont.Display,
            drawable => 0,
            context  => Null)=0 then
            raise FailedToDestroyContext
              with "Failed Call to glXMakeCurrent using drawable=0";
         end if;

         glX.glXDestroyContext
           (dpy => Cont.Display,
            ctx => Cont.GLXContext);

      end if;

      if Cont.Window/=0 then
         Xlib.XDestroyWindow
           (display => Cont.Display,
            window  => Cont.Window);
      end if;

      if Cont.Display/=null then
         Xlib.XCloseDisplay
           (display => Cont.Display);
      end if;
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
      use type Xlib.XVisualInfo_Access;
      use type Interfaces.C.int;
      use type Interfaces.C.long;
      use type Xlib.Window_Type;
      use type Xlib.Atom_Type;
      use type Xlib.Status_Type;
      use type Xlib.XIM_Access;
      use type Xlib.XIC_Access;

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

      if XLib.XSetErrorHandler
        (func => ErrorHandler'Access)=0 then
         raise FailedToCreateContext
           with "Failed call to XSetErrorHandler";
      end if;

      if glX.glXQueryVersion
        (dpy   => Context.Display,
         major => Context.GLXMajor'Access,
         minor => Context.GLXMinor'Access)=0 then

         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed call to glXQueryVersion"
             &ErrorCodeString;

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
           with "Failed call to glXChooseVisual"
             &ErrorCodeString;
      end if;

      Context.ColorMap := Xlib.XCreateColormap
        (display => Context.Display,
         window  => Xlib.RootWindow
           (display => Context.Display,
            screen  => Context.Visual.screen),
         visual  => Context.Visual.visual,
         alloc   => Xlib.AllocNone);

      declare
         Attr : aliased XLib.XSetWindowAttributes_Type;
      begin
         Attr.colormap:=Context.ColorMap;
         Attr.border_pixel:=16#FFFFFFFF#;
         -- This sum of events should work, but OR would be the better
         -- choice
         Attr.event_mask:=
           Xlib.StructureNotifyMask
           + Xlib.ExposureMask
           + Xlib.KeyPressMask
           + XLib.KeyReleaseMask
           + Xlib.ButtonPressMask
           + Xlib.ButtonReleaseMask
           + Xlib.PointerMotionMask;

         Context.Window:=Xlib.XCreateWindow
           (display => Context.Display,
            parent  => Xlib.RootWindow
              (display => Context.Display,
               screen  => Context.Visual.screen),
            x       => 0,
            y       => 0,
            width   => 640,
            height  => 480,
            border_width => 0,
            depth   => Context.Visual.depth,
            class   => Xlib.InputOutput,
            visual  => Context.Visual.visual,
            valuemask => Xlib.CWBackPixel
                        +Xlib.CWBorderPixel
                        +Xlib.CWColorMap
                        +Xlib.CWEventMask,
            attributes => Attr'Unchecked_Access);

      end;

      if Context.Window=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XCreateWindow returned no window"
             &ErrorCodeString;
      end if;

      declare
         SizeHints : aliased Xlib.XSizeHints_Type;
      begin
         SizeHints.width  := 400;
         SizeHints.height := 400;
         SizeHints.flags  := Xlib.USSize+XLib.USPosition;
         Xlib.XSetNormalHints
           (display => Context.Display,
            window  => Context.Window,
            hints   => SizeHints'Unchecked_Access);
--         Xlib.XSetStandardProperties
--           (display => Context.Display,
--            window  => Context.Window,

      end;

      declare
         AtomName : Interfaces.C.Strings.chars_ptr
           :=Interfaces.C.Strings.New_String("WM_DELETE_WINDOW");
      begin
         Context.DeleteWindowAtom:=XLib.XInternAtom
           (display => Context.Display,
            atom_name => AtomName,
            only_if_exists => 1);
         Interfaces.C.Strings.Free(AtomName);
      end;
      if Context.DeleteWindowAtom=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XInternAtom with WM_DELETE_WINDOW failed"
             &ErrorCodeString;
      end if;

      if Xlib.XSetWMProtocols
        (display   => Context.Display,
         window    => Context.Window,
         protocols => Context.DeleteWindowAtom'Access,
         count     => 1)=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XSetWMProtocols failed"
             &ErrorCodeString;
      end if;

      Context.GLXContext:=glX.glXCreateContext
        (dpy       => Context.Display,
         vis       => Context.Visual,
         shareList => null,
         direct    => 1);

      if Context=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to XCreateContext failed"
             &ErrorCodeString;
      end if;

      Xlib.XMapWindow
        (display => Context.Display,
         window  => Context.Window);

      if glX.glxMakeCurrent
        (dpy      => Context.Display,
         drawable => GLX.GLXDrawable_Type(Context.Window),
         context  => Context.GLXContext)=0 then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Call to glxMakeCurrent failed"
             &ErrorCodeString;
      end if;

      Context.InputIM:=Xlib.XOpenIM
        (display   => Context.Display,
         db        => null,
         res_name  => Interfaces.C.Strings.Null_Ptr,
         res_class => Interfaces.C.Strings.Null_Ptr);

      if Context.InputIM=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed to create input IM with XOpenIM"
             &ErrorCodeString;
      end if;

      Put("*****************************************");
      New_Line;
      Put("Find out allowed IM Values:");
      New_Line;
      declare

         use type Interfaces.C.Strings.chars_ptr;
         use type XLib.XIMStyle_Access;

         ReturnString : Interfaces.C.Strings.chars_ptr;
         styles       : aliased Xlib.XIMStyles_Access;
         cursor       : Xlib.XIMStyle_Access;

      begin
         ReturnString:=XLib.XGetIMValues_1
           (xim                 => Context.InputIM,
            im_supported_styles => styles'Unchecked_Access);
         if Interfaces.C.Strings.Null_Ptr=ReturnString then
            Put("Null Return String");
            New_Line;
         else
            Put("Return String:");
            Put(Interfaces.C.Strings.Value(ReturnString));
            New_Line;
         end if;
         Put("Number of supported styles:");
         Put(Interfaces.C.short'Image(styles.count_styles));
         New_Line;
         cursor:=styles.supported_styles;
         for i in 1..styles.count_styles loop
            Put("Style ");
            Put(Integer(i));
            Put(":");
            Put(cursor.all'Address);
            Put(":");
            Put(XLib.XIMStyle_Type'Image(cursor.all));
            New_Line;
            cursor:=cursor+Interfaces.C.size_t(Interfaces.C.long'Size/8);
         end loop;

      end;
      Put("*****************************************");
      New_Line;

      Context.InputContext:=Xlib.XCreateIC_1
        (im         => Context.InputIM,
         window     => Context.Window,
         inputstyle => Xlib.XIMPreeditNothing+Xlib.XIMStatusNothing);

      Put("Try to create Input Context");
      New_Line;
      if Context.InputContext=null then
         FreeContext(Context_ClassAccess(Context));
         raise FailedToCreateContext
           with "Failed to create Input Context with XCreateIC"
             &ErrorCodeString;
      end if;

      -- Initialize basic window environment
      GUI.Initialize
        (Context => Context_ClassAccess(Context));

      Context.ContextInitialized:=True;

      return Context_ClassAccess(Context);
   end NewContext;
   ---------------------------------------------------------------------------

   Implementation : constant Implementation_Type:=
     (NewContext  => NewContext'Access,
      FreeContext => FreeContext'Access);
   Identifier     : constant Unbounded_String:=To_Unbounded_String("OpenGL");

   procedure Register is
   begin

      Implementations.Register
        (Identifier     => Identifier,
         Implementation => Implementation);

   end Register;
   ---------------------------------------------------------------------------

   procedure UnRegister is
   begin

      Implementations.UnRegister
        (Identifier => Identifier);

   end UnRegister;

end GUI.OpenGL.Native;
