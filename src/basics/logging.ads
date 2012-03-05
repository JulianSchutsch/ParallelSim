pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config.Implementations;

package Logging is


   type Channel_Type is tagged limited null record;
   type Channel_ClassAccess is access all Channel_Type'Class;

   procedure Write
     (Item    : in out Channel_Type;
      Message : Unbounded_String) is null;

   type Channel_Constructor is
     access function
       (ModuleName : Unbounded_String;
        ChannelName : Unbounded_String)
        return Channel_ClassAccess;

   type Channel_Destructor is
     access procedure
       (Item : Channel_ClassAccess);

   type Implementation_Type is
      record
         NewChannel  : Channel_Constructor:=null;
         FreeChannel : Channel_Destructor:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey => To_Unbounded_String("LoggingImplementation"));

end Logging;
