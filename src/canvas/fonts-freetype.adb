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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with Interfaces.C.Strings;
with Fonts.FreeType.Large; use Fonts.FreeType.Large;
with Fonts.Freetype.Small; use Fonts.FreeType.Small;

package body Fonts.Freetype is

   Initialized : Boolean:=False;

   function Requester
     (face_id      : FTC_FaceID_Type;
      library      : FT_Library_Access;
      request_data : FT_Pointer_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type;
   pragma Convention(C,Requester);
   ---------------------------------------------------------------------------

   function Height
     (Font : access FreeTypeFont_Type)
      return Integer is
   begin
      return Font.LineHeight;
   end Height;
   ---------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => FreeTypeFont_Type'Class,
      Name   => FreeTypeFont_ClassAccess);

   procedure Unload
     (Font : Font_ClassAccess) is
   begin
      FTC_Manager_RemoveFaceID
        (manager => Manager,
         face_id => FTC_FaceID_Type(Font.all'Address));
   end Unload;
   ---------------------------------------------------------------------------

   function Load
     (Name       : Unbounded_String;
      Size       : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess is

      pragma Unreferenced(Attributes);

      Font   : FreeTypeFont_ClassAccess;
      Error  : FT_Error_Type;

   begin
      -- Being here means we need to create a new font with specific size
      -- we do not need to do any lookups
      if Size>20 then
         declare
            LargeFont : LargeFont_Access;
         begin
            LargeFont := new LargeFont_Type;
            Font      := FreeTypeFont_ClassAccess(LargeFont);
         end;
      else
         declare
            SmallFont : SmallFont_Access;
         begin
            SmallFont := new SmallFont_Type;
            Font      := FreeTypeFont_ClassAccess(SmallFont);
         end;
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
         -- TODO : This should be replaced by an Exception, but take care of the calling
         -- function first
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
         Font.LineHeight:=Integer((FaceSize.metrics.ascender
           -FaceSize.metrics.descender)/64);

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

      Fonts.Register
        (Load   => Load'Access,
         Unload => Unload'Access);

      Initialized:=True;
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin

      if Initialized then
         Fonts.UnRegister(Load => Load'Access);
      end if;

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
