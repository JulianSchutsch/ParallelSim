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
--   30.Jun 2012 Julian Schutsch
--     - Original version

pragma Ada_2005;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config.Implementations;
with Basics; use Basics;

package Authentication is

   type System_Type is abstract tagged null record;

   type PublicKey_Type is abstract tagged null record;
   type PrivateKey_Type is abstract tagged null record;

   type PublicKey_ClassAccess is access all PublicKey_Type;
   type PrivateKey_ClassAccess is access all PrivateKey_Type;
   type System_ClassAccess is access all System_Type;

   procedure GenerateKeyPair
     (System     : access System_Type;
      PublicKey  : out PublicKey_ClassAccess;
      PrivateKey : out PrivateKey_ClassAccess) is abstract;

   function GenerateMessage
     (System : access System_Type)
      return Unbounded_String is abstract;

   function Encrypt
     (PrivateKey : access PrivateKey_Type;
      Message    : Unbounded_String)
      return Unbounded_String is abstract;

   function Verify
     (PublicKey  : access PublicKey_Type;
      Message    : Unbounded_String;
      Encrypted  : Unbounded_String)
      return Boolean is abstract;

   type Implementation_Type is
      record
         System : System_ClassAccess:=null;
      end record;

   package Implementations is new Config.Implementations
     (Implementation_Type => Implementation_Type,
      IdentifierKey       => U("AuthenticationType"));

end Authentication;
