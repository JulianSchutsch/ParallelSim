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
with Fonts.Freetype.Thin; use Fonts.Freetype.Thin;
with System;
with Interfaces.C.Strings;

package body Fonts.Freetype is

   Library    : aliased FT_Library_Access     := null;
   Manager    : aliased FTC_Manager_Access    := null;
   SBitCache  : aliased FTC_SBitCache_Access  := null;
   ImageCache : aliased FTC_ImageCache_Access := null;
   CMapCache  : aliased FTC_CMapCache_Access  := null;

   VersionMajor : aliased FT_Int_Type;
   VersionMinor : aliased FT_Int_Type;
   VersionPatch : aliased FT_Int_Type;

   function FreeTypeLookup
     (Name : Unbounded_String;
      Size : Natural;
      Attributes : Attributes_Type)
      return Font_ClassAccess is
      pragma Unreferenced(Name);
      pragma Unreferenced(Size);
      pragma Unreferenced(Attributes);

   begin
      return null;
   end FreeTypeLookup;
   ---------------------------------------------------------------------------

   function Requester
     (face_id      : FTC_FaceID_Type;
      library      : FT_Library_Access;
      request_data : FT_Pointer_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type;
   pragma Convention(C,Requester);

   type Request_Type is
      record
         Filename : Interfaces.C.Strings.chars_ptr;
         Index    : FT_Long_Type;
      end record;
   type Request_Access is access all Request_Type;

   function Requester
     (face_id      : FTC_FaceID_Type;
      library      : FT_Library_Access;
      request_data : FT_Pointer_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type is
      pragma Unreferenced(face_id);

      function Convert is new Ada.Unchecked_Conversion
        (Source => FT_Pointer_Type,
         Target => Request_Access);

      RequestData : constant Request_Access:=Convert(request_data);

   begin
      return FT_New_Face
        (library      => library,
         filepathname => RequestData.Filename,
         face_index   => RequestData.Index,
         aface        => aface);
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
        (LookupFunction => FreeTypeLookup'Access);
   end Initialize;
   ---------------------------------------------------------------------------

   procedure Finalize is
   begin
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
