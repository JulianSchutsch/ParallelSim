-------------------------------------------------------------------------------
--   Copyright 2012 Julian Schutsch
--
--   This file is part of ParallelSim
--
--   ParallelSim is free software: you can redistribute it and/or modify
--   it under the terms of the GNU Affero General Public License as published
--   by the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   ParallelSim is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU Affero General Public License for more details.
--
--   You should have received a copy of the GNU Affero General Public License
--   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Fonts.Freetype.Thin; use Fonts.Freetype.Thin;
with System;
with Interfaces.C.Strings;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Basics; use Basics;

package body Fonts.Freetype is

   Library    : aliased FT_Library_Access     := null;
   Manager    : aliased FTC_Manager_Access    := null;
   SBitCache  : aliased FTC_SBitCache_Access  := null;
   ImageCache : aliased FTC_ImageCache_Access := null;
   CMapCache  : aliased FTC_CMapCache_Access  := null;

   VersionMajor : aliased FT_Int_Type;
   VersionMinor : aliased FT_Int_Type;
   VersionPatch : aliased FT_Int_Type;

   function Requester
     (face_id      : FTC_FaceID_Type;
      library      : FT_Library_Access;
      request_data : FT_Pointer_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type;
   pragma Convention(C,Requester);
   ---------------------------------------------------------------------------

   type GlyphArray_Type is array(Natural range <>) of FT_UInt_Type;
   type GlyphArray_Access is access GlyphArray_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => GlyphArray_Type,
      Name   => GlyphArray_Access);

   type FreeTypeFont_Type;
   type FreeTypeFont_ClassAccess is access all FreeTypeFont_Type'Class;
   type FreeTypeFont_Type is abstract new Fonts.Font_Type with
      record
         Filename   : Unbounded_String;
         Index      : FT_Long_Type;
         FaceHandle : aliased FT_Face_Access;
         Scaler     : aliased FTC_Scaler_Type;
         BaseLine   : Integer;
      end record;
   ---------------------------------------------------------------------------

   Glyphs     : GlyphArray_Access:=null;
   Node       : aliased FTC_Node_Access;
   Glyph      : aliased FT_Glyph_Access;


   type LargeFont_Type;
   type LargeFont_Access is access all LargeFont_Type;

   type LargeFont_Type is new FreeTypeFont_Type with
      record
         null;
      end record;

   overriding
   function TextWidth
     (Font : access LargeFont_Type;
      Text : Unbounded_String)
      return Integer;

   overriding
   procedure TextOut
     (Font   : access LargeFont_Type;
      Canvas : Standard.Canvas.BasicCanvas_ClassAccess;
      X      : Integer;
      Y      : Integer;
      Text   : Unbounded_String;
      Color  : Standard.Canvas.Color_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => FreeTypeFont_Type'Class,
      Name   => FreeTypeFont_ClassAccess);
   ---------------------------------------------------------------------------

   procedure DecodeString
     (Font : access LargeFont_Type;
      Text : Unbounded_String) is

      UC4Text    : Unbounded_Wide_Wide_String;

   begin

      UC4Text:=UTF8ToUC4(Text);

      if Glyphs/=null then
         Free(Glyphs);
      end if;

      Glyphs:=new GlyphArray_Type(1..Length(UC4Text));

      for i in 1..Length(UC4Text) loop

         Glyphs(i):=FTC_CMapCache_Lookup
           (cache      => CMapCache,
            face_id    => FTC_FaceID_Type(Font.all'Address),
            cmap_index => -1,
            char_code  => Wide_Wide_Character'Pos(Element(UC4Text,i)));

      end loop;

   end DecodeString;
   ---------------------------------------------------------------------------

   procedure SelectGlyph
     (Font      : access LargeFont_Type;
      Character : FT_UInt_Type) is

      Error : FT_Error_Type;

   begin
      if Node/=null then
         FTC_Node_Unref(Node,Manager);
      end if;
      Error:=FTC_ImageCache_LookupScaler
        (cache      => ImageCache,
         scaler     => Font.Scaler'Access,
         load_flags => FT_LOAD_DEFAULT,
         gindex     => Character,
         aglyph     => Glyph'Access,
         anode      => Node'Access);
      if Error/=0 then
         raise FailedRendering
           with "Failed call to FTC_ImageCache_LookupScaler, exit code:"
             &FT_Error_Type'Image(Error);
      end if;
   end SelectGlyph;

   function TextWidth
     (Font : access LargeFont_Type;
      Text : Unbounded_String)
      return Integer is
   begin
      DecodeString(Font,Text);
      return 0;
   end TextWidth;
   ---------------------------------------------------------------------------

   procedure GlyphOut
     (Font : access LargeFont_Type;
      Canvas     : Standard.Canvas.BasicCanvas_ClassAccess;
      X          : in out Integer;
      Y          : in out Integer;
      GlyphIndex : FT_UInt_Type;
      Color      : Standard.Canvas.Color_Type) is

      X1 : Integer;
      Y1 : Integer;
      X2 : Integer;
      Y2 : Integer;

      Error : FT_Error_Type;
      Bitmap : FT_BitmapGlyph_Access;
      Width : Integer;
      Height : Integer;
      SourceOrigin : Integer:=0;
      SourceAdd    : Integer:=0;
      Gray : Integer;
      SourcePointer : GrayValue_Access;

   begin
      SelectGlyph(Font,GlyphIndex);

      Error:=FT_Glyph_To_Bitmap
        (the_glyph   => Glyph'Access,
         render_mode => FT_RENDER_MODE_NORMAL,
         origin      => null,
         destroy     => 0);
      if Error/=0 then
         raise FailedRendering
           with "Failed call to FT_Glyph_To_Bitmap, Exit code:"
             &FT_Error_Type'Image(Error);
      end if;
      Bitmap:=Convert(Glyph);
      Height := Integer(Bitmap.bitmap.rows);
      Width  := Integer(Bitmap.bitmap.width);
      if (Height<0) or (Width<0) then
         raise FailedRendering
           with "Encountered Bitmap with negative Height or Width";
      end if;
      -- Data in Bitmap.Bitmap.buffer
      -- Advance = Glyph.advance.x div 1024 div 64
      -- Top = lbaseline-Bitmap.Top
      -- Left = Bitmap.Left
      X1 := X+Integer(Bitmap.left);
      Y1 := Y+Font.BaseLine-Integer(Bitmap.top);
      X2 := X1+Width-1;
      Y2 := Y1+Height-1;
      Put("COND");
      Put(X1);
      Put(Canvas.ContentWidth);
      New_Line;
      if (X2>=0)
        and (X1<Canvas.ContentWidth)
        and (Y2>=0)
        and (Y1<Canvas.ContentHeight) then
         Put("DRAW");
         New_Line;

         if X1<0 then
            SourceOrigin := -X1;
            SourceAdd    := -X1;
            X1:=0;
         end if;
         if Y1<0 then
            SourceOrigin := SourceOrigin-Y1*Width;
            Y1:=0;
         end if;

         if X2>Canvas.ContentWidth then
            SourceAdd:=SourceAdd+X2-Canvas.ContentWidth+1;
            X2:=Canvas.ContentWidth-1;
         end if;

         if Y2>=Canvas.ContentHeight then
            Y2:=Canvas.ContentHeight-1;
         end if;
         Put("MaxY");
         Put(Y2);
         New_Line;

         SourcePointer:=Bitmap.bitmap.buffer+Interfaces.C.size_t(SourceOrigin);
         for n in Y1..Y2 loop
            for i in X1..X2 loop
               Gray:=Integer(SourcePointer.all);
               if Gray/=0 then
                  Canvas.Image(n,i):=Standard.Canvas.PreBlendMix
                    (BackgroundColor => Canvas.Image(n,i),
                     ForegroundColor => Standard.Canvas.MultiplyAlpha(Color,Gray));
               end if;
               SourcePointer:=SourcePointer+1;

            end loop;
            SourcePointer:=SourcePointer+Interfaces.C.size_t(SourceAdd);
         end loop;

      end if;

      X:=X+Integer(Glyph.advance.x/(64*1024));
      Y:=Y+Integer(Glyph.advance.y/(64*1024));
   end GlyphOut;
   ---------------------------------------------------------------------------

   procedure TextOut
     (Font   : access LargeFont_Type;
      Canvas : Standard.Canvas.BasicCanvas_ClassAccess;
      X      : Integer;
      Y      : Integer;
      Text   : Unbounded_String;
      Color  : Standard.Canvas.Color_Type) is

      XPosition : Integer;
      YPosition : Integer;

   begin
      DecodeString(Font,Text);

      XPosition := X;
      YPosition := Y;

      for i in Glyphs'Range loop

         Put("Index : ");
         Put(Integer(Glyphs(i)));
         New_Line;
         GlyphOut
           (Font        => Font,
            Canvas      => Canvas,
            X           => XPosition,
            Y           => YPosition,
            GlyphIndex  => Glyphs(i),
            Color       => Color);

      end loop;

   end TextOut;
   ---------------------------------------------------------------------------

   function Load
     (Name : Unbounded_String;
      Size : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess is

      pragma Unreferenced(Attributes);

      Font   : FreeTypeFont_ClassAccess;
      Error  : FT_Error_Type;

   begin
      -- Being here means we need to create a new font with specific size
      -- we do not need to do any lookups
      Put("Load:");
      Put(To_String(Name));
      New_Line;
      if Size>0 then
         declare
            LargeFont : LargeFont_Access;
         begin
            LargeFont := new LargeFont_Type;
            Font:=FreeTypeFont_ClassAccess(LargeFont);
         end;
      else
         null; -- Load small fonts
         return null;
      end if;

      -- Don't setup any private parts of the Font, since this is handled
      -- by the parent package
      Font.Filename := Name;
      Font.Index    := 0;

      Error:=FTC_Manager_LookupFace
        (manager => Manager,
         face_id => FTC_FaceID_Type(Font.all'Address),
         aface   => Font.FaceHandle'Access);
      if Error/=0 then
         Put("Failed LookupFace");
         Put(FT_Error_Type'Image(Error));
         New_Line;
         Free(Font);
         return null;
      end if;

      Font.Scaler.face_id := FTC_FaceID_Type(Font.all'Address);
      Font.Scaler.width   := FT_UInt_Type(Size);
      Font.Scaler.height  := FT_UInt_Type(Size);
      Font.Scaler.pixel   := 1;
      Font.Scaler.x_res   := 0;
      Font.Scaler.y_res   := 0;

      declare
         FaceSize : aliased FT_Size_Access;
      begin

         Error:=FTC_Manager_LookupSize
           (manager => Manager,
            -- Local variable used as buffer for c function.
            -- Won't be used after return from this function.
            scaler  => Font.Scaler'Access,
            asize   => FaceSize'Access);
         if Error/=0 then
            Put("Failed LookupSize");
            New_Line;
            Free(Font);
            return null;
         end if;

         Font.BaseLine:=Integer(FaceSize.metrics.ascender/64);

         Put("Baseline:");
         Put(Font.BaseLine);
         New_Line;

      end;

      return Font_ClassAccess(Font);
   end Load;
   ---------------------------------------------------------------------------

   function Requester
     (face_id      : FTC_FaceID_Type;
      library      : FT_Library_Access;
      request_data : FT_Pointer_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type is
      pragma Unreferenced(request_data);

      function Convert is new Ada.Unchecked_Conversion
        (Source => FTC_FaceID_Type,
         Target => FreeTypeFont_ClassAccess);

      Font      : constant FreeTypeFont_ClassAccess:=Convert(face_id);
      CFileName : Interfaces.C.Strings.chars_ptr;
      Result    : FT_Error_Type;

   begin
      Put("Requester:");
      Put(To_String(Font.Name));
      New_Line;

      CFileName := Interfaces.C.Strings.New_String(To_String(Font.Filename));
      Result:=FT_New_Face
        (library      => library,
         filepathname => CFileName,
         face_index   => Font.Index,
         aface        => aface);
      Interfaces.C.Strings.Free(CFileName);

      return Result;

   end Requester;
   ---------------------------------------------------------------------------

   procedure Initialize is
      Error : FT_Error_Type;

   begin
      Put("Initialize FreeType");
      New_Line;

      Error:=FT_Init_FreeType
        (Library => Library'Access);
      if Error/=0 then
         raise FailedFontImplementationInitialization
           with "Failed call to FT_Init_FreeType";
      end if;
      FT_Library_Version
        (Library => Library,
         Major   => VersionMajor'Access,
         Minor   => VersionMinor'Access,
         Patch   => VersionPatch'Access);
      Put("FreeType ");
      Put(Integer(VersionMajor));
      Put(Integer(VersionMinor));
      Put(Integer(VersionPatch));
      New_Line;
      if not
        ((VersionMajor>2)
         or ((VersionMajor=2) and (VersionMinor>2))) then
         Finalize;
         raise FailedFontImplementationInitialization
           with "Require higher version of FreeType, at least 2.3, found "
             &FT_Int_Type'Image(VersionMajor)&"."
             &FT_Int_Type'Image(VersionMinor)&"."
             &FT_Int_Type'Image(VersionPatch);
         return;
      end if;

      Error:=FTC_Manager_New
        (Library   => Library,
         max_faces => 0,
         max_sizes => 0,
         max_bytes => 0,
         requester => Requester'Access,
         req_data  => System.Null_Address,
         amanager  => Manager'Access);
      if Error/=0 then
         Finalize;
         raise FailedFontImplementationInitialization
           with "Failed call to FTC_Manager_New";
      end if;

      Error:=FTC_SBitCache_New
        (manager => Manager,
         acache  => SBitCache'Access);
      if Error/=0 then
         Finalize;
         raise FailedFontImplementationInitialization
           with "Failed call to FTC_SBitCache_New";
      end if;

      Error:=FTC_ImageCache_New
        (manager => Manager,
         acache  => ImageCache'Access);
      if Error/=0 then
         Finalize;
         raise FailedFontImplementationInitialization
           with "Failed call to FTC_ImageCache_New";
      end if;

      Error:=FTC_CMapCache_New
        (manager => Manager,
         acache  => CMapCache'Access);
      if Error/=0 then
         Finalize;
         raise FailedFontImplementationInitialization
           with "Failed call to FTC_CMapCache_New";
      end if;

      Put("Register FreeType");
      New_Line;
      Fonts.Register
        (Load => Load'Access);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
      if Node/=null then
         FTC_Node_Unref(Node,Manager);
      end if;
      if Manager/=null then
         FTC_Manager_Done(Manager);
         Manager:=null;
      end if;
      if Library/=null then
         FT_Done_FreeType(Library);
         Library:=null;
      end if;
   end Finalize;
   ---------------------------------------------------------------------------

end Fonts.Freetype;
