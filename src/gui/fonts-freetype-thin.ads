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

-- Revision History
--   30.Mar 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Fonts.Freetype.Thin is

   type FT_Library_Opaque is null record;
   type FT_Library_Access is access FT_Library_Opaque;
   type FT_Int_Type is new Interfaces.C.int;
   type FT_UInt_Type is new Interfaces.C.unsigned;
   type FT_ULong_Type is new Interfaces.C.unsigned_long;
   type FT_Long_Type is new Interfaces.C.long;
   type FT_Error_Type is new Interfaces.C.int;

   type FTC_Manager_Opaque is null record;
   type FTC_Manager_Access is access FTC_Manager_Opaque;
   type FTC_SBitCache_Opaque is null record;
   type FTC_SBitCache_Access is access FTC_SBitCache_Opaque;
   type FTC_ImageCache_Opaque is null record;
   type FTC_ImageCache_Access is access FTC_ImageCache_Opaque;
   type FTC_CMapCache_Opaque is null record;
   type FTC_CMapCache_Access is access FTC_CMapCache_Opaque;
   subtype FT_Pointer_Type is System.Address;
   type FTC_FaceID_Type is new FT_Pointer_Type;
   type FT_Face_Opaque is null record;
   type FT_Face_Access is access FT_Face_Opaque;

   type FTC_Face_Requester_Access is
     access function
       (face_id      : FTC_FaceID_Type;
        library      : FT_Library_Access;
        request_data : FT_Pointer_Type;
        aface        : access FT_Face_Access)
        return FT_Error_Type;
   pragma Convention(C,FTC_Face_Requester_Access);

   type FTC_ScalerRec is
      record
         face_id : FTC_FaceID_Type;
         width   : FT_UInt_Type;
         height  : FT_UInt_Type;
         pixel   : FT_Int_Type;
         x_res   : FT_UInt_Type;
         y_res   : FT_UInt_Type;
      end record;
   pragma Convention(C,FTC_ScalerRec);

   function FT_Init_FreeType
     (Library : access FT_Library_Access)
      return FT_Error_Type;
   pragma Import(C,FT_Init_FreeType,"FT_Init_FreeType");

   procedure FT_Library_Version
     (Library : FT_Library_Access;
      Major   : access FT_Int_Type;
      Minor   : access FT_Int_Type;
      Patch   : access FT_Int_Type);
   pragma Import(C,FT_Library_Version,"FT_Library_Version");

   procedure FT_Done_FreeType
     (Library : FT_Library_Access);
   pragma Import(C,FT_Done_FreeType,"FT_Done_FreeType");

   function FTC_Manager_New
     (library   : FT_Library_Access;
      max_faces : FT_UInt_Type;
      max_Sizes : FT_UInt_Type;
      max_bytes : FT_ULong_Type;
      requester : FTC_Face_Requester_Access;
      req_data  : FT_Pointer_Type;
      amanager  : access FTC_Manager_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_Manager_New,"FTC_Manager_New");

   function FTC_SBitCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_SBitCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_SBitCache_New,"FTC_SBitCache_New");

   function FTC_ImageCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_ImageCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_ImageCache_New,"FTC_ImageCache_New");

   function FTC_CMapCache_New
     (manager : FTC_Manager_Access;
      acache  : access FTC_CMapCache_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_CMapCache_New,"FTC_CMapCache_New");

   procedure FTC_Manager_Done
     (manager : FTC_Manager_Access);
   pragma Import(C,FTC_Manager_Done,"FTC_Manager_Done");

   function FT_New_Face
     (library      : FT_Library_Access;
      filepathname : Interfaces.C.Strings.chars_ptr;
      face_index   : FT_Long_Type;
      aface        : access FT_Face_Access)
      return FT_Error_Type;
   pragma Import(C,FT_New_Face,"FT_New_Face");

   function FTC_Manager_LookupFace
     (manager : FTC_Manager_Access;
      face_id : FTC_FaceID_Type;
      aface   : access FT_Face_Access)
      return FT_Error_Type;
   pragma Import(C,FTC_Manager_LookupFace,"FTC_Manager_LookupFace");

end Fonts.Freetype.Thin;
