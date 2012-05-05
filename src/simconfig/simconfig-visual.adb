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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body SimConfig.Visual is

   procedure ArrangeOptionElements
     (ElementsPage : access ElementsPage_Type) is

      use type GUI.Label.Label_ClassAccess;
      use type GUI.Edit.Edit_ClassAccess;
      use type GUI.ComboBox.Combobox_ClassAccess;

--      TabBounds : constant Bounds_Type:=Tab.Tab.GetBounds;

      procedure ArrangeElements
        (X        : Integer;
         Y        : in out Integer;
         Elements : ElementArray_Access) is
      begin

         if Elements=null then
            return;
         end if;
         for i in Elements'Range loop
            if Elements(i).Label/=null then
               Elements(i).Label.SetBounds
                 (Top     => Y,
                  Left    => X,
                  Height  => 30,
                  Width   => 400,
                  Visible => True);
               Y:=Y+27;
            end if;
            if Elements(i).Edit/=null then
               Elements(i).Edit.SetBounds
                 (Top     => Y,
                  Left    => X,
                  Height  => 30,
                  Width   => 400,
                  Visible => True);
               Y:=Y+35;
            end if;
            if Elements(i).Combobox/=null then
               Elements(i).Combobox.SetBounds
                 (Top     => Y,
                  Left    => X,
                  Height  => 30,
                  Width   => 400,
                  Visible => True);
               Y:=Y+35;
            end if;
            if Elements(i).Elements/=null then
               ArrangeElements
                 (X        => X+10,
                  Y        => Y,
                  Elements => Elements(i).Elements);
            end if;
         end loop;

      end ArrangeElements;
      ------------------------------------------------------------------------

      Y : Integer:=5;

   begin
      ArrangeElements
        (X        => 5,
         Y        => Y,
         Elements => ElementsPage.Elements);
   end ArrangeOptionElements;
   ---------------------------------------------------------------------------

   procedure DeleteOptionElements
     (Elements : in out ElementArray_Access) is

      use type GUI.Label.Label_ClassAccess;
      use type GUI.Edit.Edit_ClassAccess;
      use type GUI.ComboBox.ComboBox_ClassAccess;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => ElementArray_Type,
         Name   => ElementArray_Access);
   begin
      if Elements=null then
         return;
      end if;
      Put("DOE");
      for i in Elements'Range loop
         Put("Delete Component");
         if Elements(i).Label/=null then
            Elements(i).Label.Free;
         end if;
         Put("A");
         if Elements(i).Edit/=null then
            Elements(i).Edit.Free;
         end if;
         Put("B");
         if Elements(i).Combobox/=null then
            Elements(i).Combobox.Free;
         end if;
         Put("Delete Sub Array");
         if Elements(i).Elements/=null then
            DeleteOptionElements(Elements(i).Elements);
         end if;
      end loop;
      Free(Elements);
      Put("//");
   end DeleteOptionElements;
   ---------------------------------------------------------------------------

   procedure CreateOptionElements
     (ElementsPage : access ElementsPage_Type;
      Options      : SimConfig.ConfigArray_Access;
      Elements     : out ElementArray_Access);

   procedure ComboBoxSelect
     (CallBackObject : AnyObject_ClassAccess) is

      Element : constant Element_Access:=Element_Access(CallBackObject);

   begin
      DeleteOptionElements(Element.Elements);
      Put("ComboBoxSelect :");
      Put(Element.ComboBox.GetIndex);
      New_Line;
      CreateOptionElements
        (ElementsPage => Element.ElementsPage,
         Options      => Element.Option.Set(Element.ComboBox.GetIndex+Element.Option.Set'First).Options,
         Elements     => Element.Elements);
      ArrangeOptionElements(Element.ElementsPage);
   end ComboBoxSelect;
   ---------------------------------------------------------------------------

   procedure CreateOptionElements
     (ElementsPage : access ElementsPage_Type;
      Options      : SimConfig.ConfigArray_Access;
      Elements     : out ElementArray_Access) is

      use type SimConfig.ConfigArray_Access;

   begin
      if Options=null then
         Elements:=null;
         return;
      end if;
      Elements:=new ElementArray_Type(Options'Range);
      Put_Line("Create Elements");
      Put(Elements'First);
      Put(Elements'Last);
      New_Line;
      for i in Elements'Range loop
         Elements(i).ElementsPage    := ElementsPage_Access(ElementsPage);
         Elements(i).Option := Options(i)'Access;
         case Options(i).TType is
            when SimConfig.ConfigElemString =>
               Elements(i).Edit:=ElementsPage.Theme.NewEdit(GUI.Object_ClassAccess(ElementsPage));
               Elements(i).Edit.SetText(Options(i).Default);
               Elements(i).Label:=ElementsPage.Theme.NewLabel(GUI.Object_ClassAccess(ElementsPage));
               Elements(i).Label.SetCaption(Options(i).Description);

            when SimConfig.ConfigElemChoice =>
               Elements(i).Combobox:=ElementsPage.Theme.NewCombobox(GUI.Object_ClassAccess(ElementsPage));
               Elements(i).Combobox.CallBackObject:=Elements(i)'Access;
               Elements(i).Label:=ElementsPage.Theme.NewLabel(GUI.Object_ClassAccess(ElementsPage));
               Elements(i).Label.SetCaption(Options(i).Description);
               for n in Options(i).Set'Range loop
                  Elements(i).Combobox.AddEntry
                    (String => Options(i).Set(n).Description,
                     Color  => 16#FFFFFFFF#);
               end loop;
               Elements(i).Combobox.SetIndex(Options(i).DefaultIndex);
               CreateOptionElements
                 (ElementsPage => ElementsPage,
                  Options      => Options(i).Set(Options(i).DefaultIndex+Options(i).Set'First).Options,
                  Elements     => Elements(i).Elements);
               Elements(i).Combobox.OnSelect:=ComboBoxSelect'Access;

            when SimConfig.ConfigElemConstantString =>
               null;
         end case;
      end loop;
   end CreateOptionElements;
   ---------------------------------------------------------------------------

   function CreateElementsPage
     (Parent    : GUI.Object_ClassAccess;
      Options   : ConfigArray_Access;
      Theme     : GUI.Themes.Implementation_Type)
      return ElementsPage_Access is

      ElementsPage : ElementsPage_Access;

   begin

      ElementsPage:=new ElementsPage_Type;
      GUI.Object_Access(ElementsPage).Initialize(Parent);
      ElementsPage.Theme:=Theme;
      CreateOptionElements
        (ElementsPage => ElementsPage,
         Options      => Options,
         Elements     => ElementsPage.Elements);
      ArrangeOptionElements(ElementsPage);
      return ElementsPage;

   end CreateElementsPage;
   ---------------------------------------------------------------------------

   procedure Free
     (Item : access ElementsPage_Type) is

      procedure FreeMemory is new Ada.Unchecked_Deallocation
        (Object => ElementsPage_Type,
         Name   => ElementsPage_Access);

      ItemVal : ElementsPage_Access:=ElementsPage_Access(Item);

   begin
      DeleteOptionElements(Item.Elements);
      FreeMemory(ItemVal);
   end Free;
   ---------------------------------------------------------------------------

end SimConfig.Visual;