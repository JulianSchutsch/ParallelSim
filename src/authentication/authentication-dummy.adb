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

package body Authentication.Dummy is

   type PublicKey_Type is new Authentication.PublicKey_Type with null record;
   type PublicKey_Access is access all PublicKey_Type;

   overriding
   function Verify
     (PublicKey : access PublicKey_Type;
      Message   : Unbounded_String;
      Encrypted : Unbounded_String)
      return Boolean;
   ---------------------------------------------------------------------------

   type PrivateKey_Type is new Authentication.PrivateKey_Type with null record;
   type PrivateKey_Access is access all PrivateKey_Type;

   overriding
   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String;
   ---------------------------------------------------------------------------

   type System_Type is new Authentication.System_Type with null record;

   overriding
   procedure GenerateKeyPair
     (System     : access System_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess);

   overriding
   function GenerateMessage
     (System : access System_Type)
      return Unbounded_String;
   ---------------------------------------------------------------------------

   function GenerateMessage
     (System : access System_Type)
      return Unbounded_String is

      pragma Unreferenced(System);

   begin
      return U("Dummy-Message");
   end GenerateMessage;
   ---------------------------------------------------------------------------

   procedure GenerateKeyPair
     (System     : access System_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess) is

      pragma Unreferenced(System);

      NewPublicKey  : PublicKey_Access;
      NewPrivateKey : PrivateKey_Access;

   begin
      NewPublicKey  := new PublicKey_Type;
      NewPrivateKey := new PrivateKey_Type;
      PublicKey  := PublicKey_ClassAccess(NewPublicKey);
      PrivateKey := PrivateKey_ClassAccess(NewPrivateKey);
   end GenerateKeyPair;
   ---------------------------------------------------------------------------

   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String is

      pragma Unreferenced(PrivateKey);

   begin
      return Message;
   end Encrypt;
   ---------------------------------------------------------------------------

   function Verify
     (PublicKey : access PublicKey_Type;
      Message   : Unbounded_String;
      Encrypted : Unbounded_String)
      return Boolean is

      pragma Unreferenced(PublicKey);

   begin
      return Message=Encrypted;
   end Verify;
   ---------------------------------------------------------------------------

   type System_Access is access all System_Type;
   System        : aliased System_Type;
   SystemAccess  : constant System_Access:=System'Access;
   SystemCAccess : constant System_ClassAccess:=System_ClassAccess(SystemAccess);

   Implementation : constant Implementation_Type:=
     (System => SystemCAccess);
   Identifier :constant Unbounded_String := U("Dummy");

   procedure Register is
   begin

      Implementations.Register
        (Implementation =>  Implementation,
         Identifier     => Identifier);

   end Register;
   ---------------------------------------------------------------------------

   procedure Unregister is
   begin

      Implementations.Unregister(Identifier);

   end Unregister;
   ---------------------------------------------------------------------------

end Authentication.Dummy;
