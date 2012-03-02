package body Basics is

   function ReturnSomeString
     (Value : Integer)
      return Unbounded_String is
   begin
      return To_Unbounded_String(Integer'Image(Value));
   end ReturnSomeString;
   ---------------------------------------------------------------------------

end Basics;
