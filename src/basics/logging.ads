pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config.Implementations;

package Logging is

   type Level_Enum is
     (LevelException,
      LevelFailure,
      LevelDebug,
      LevelEvent,
      LevelRareEvent,
      LevelCommonEvent);

   type Channel_Type is abstract tagged limited null record;
   type Channel_ClassAccess is access all Channel_Type'Class;

   procedure Write
     (Item    : in out Channel_Type;
      Level   : Level_Enum;
      Message : String) is abstract;

   procedure FreeChannel
     (Item : not null access Channel_Type) is abstract;
   ---------------------------------------------------------------------------

   type Context_Type is abstract tagged limited null record;
   type Context_ClassAccess is access all Context_Type'Class;

   procedure NewChannel
     (Item        : in out Context_Type;
      ChannelName : Unbounded_String;
      Channel     : out Channel_ClassAccess) is abstract;
   ---------------------------------------------------------------------------

   type Context_Constructor is
     access function
       (Configuration : Config.Config_Type;
        ModuleName    : Unbounded_String)
        return Context_ClassAccess;

   type Context_Destructor is
     access procedure
       (Item : Context_ClassAccess);

   type Implementation_Type is
      record
         NewContext  : Context_Constructor:=null;
         FreeContext : Context_Destructor:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => To_Unbounded_String("Implementation"));

end Logging;
