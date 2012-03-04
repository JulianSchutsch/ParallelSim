pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;

package body Config.Implementations is

   type ImplementationDesc_Type is
      record
         Identifier     : Unbounded_String;
         Implementation : Implementation_Type;
      end record;

   package List_Pack is
     new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => ImplementationDesc_Type,
      "=" => "=");

   List : List_Pack.List;

   procedure Register
     (Identifier     : Unbounded_String;
      Implementation : Implementation_Type) is

   begin

      List.Append
        (New_Item =>
           (Identifier     => Identifier,
            Implementation => Implementation));

   end Register;
   ---------------------------------------------------------------------------

   function Find
     (Configuration : Config_Type;
      ModuleName    : Unbounded_String)
      return Implementation_Type is

      use type List_Pack.Cursor;

      ModuleMap          : access StringStringMap.Map;
      IdentifierValue    : Unbounded_String;
      Cursor             : List_Pack.Cursor;
      ImplementationDesc : ImplementationDesc_Type;

   begin
      ModuleMap:=GetModuleMap
        (Item => Configuration,
         Name => ModuleName);
      IdentifierValue:=ModuleMap.Element
        (Key => IdentifierKey);

      Cursor:=List.First;
      while Cursor/=List_Pack.No_Element loop
         ImplementationDesc:=List_Pack.Element(Cursor);
         if ImplementationDesc.Identifier=IdentifierValue then
            return ImplementationDesc.Implementation;
         end if;
         Cursor :=List_Pack.Next(Cursor);
      end loop;

      raise ImplementationNotFound;

   end Find;
   ---------------------------------------------------------------------------

end Config.Implementations;
