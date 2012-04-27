pragma Ada_2005;

with Ada.Containers.Doubly_Linked_Lists;

package GUI.Basics is

   type StringAndColor_Type is
      record
         String : Unbounded_String;
         Color  : Canvas.Color_Type;
      end record;

   package StringAndColorList_Pack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => StringAndColor_Type,
      "="          => "=");

end GUI.Basics;
