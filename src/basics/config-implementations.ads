pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type Implementation_Type is private;
   IdentifierKey : Unbounded_String;

package Config.Implementations is

   ImplementationNotFound : Exception;

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type);

   function Find
     (Configuration : Config_Type;
      ModuleName    : Unbounded_String)
      return Implementation_Type;

end Config.Implementations;
