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

package body ProcessLoop is

   type ProcEntry;
   type ProcEntryAccess is access ProcEntry;

   type ProcEntry is
      record
         Proc : ProcAccess;
         Next : ProcEntryAccess;
         Last : ProcEntryAccess:=null;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => ProcEntry,
      Name   => ProcEntryAccess);

   Procs : ProcEntryAccess:=null;

   procedure Process is
      Entr : ProcEntryAccess:=Procs;
   begin
      while Entr/=null loop
         Entr.Proc.all;
         Entr:=Entr.Next;
      end loop;
   end Process;
   ---------------------------------------------------------------------------

   procedure Add
     (Proc : ProcAccess) is
      NewEntry : ProcEntryAccess;
   begin
      NewEntry      := new ProcEntry;
      NewEntry.Proc := Proc;
      NewEntry.Next := Procs;

      if Procs/=null then
         Procs.Last:=NewEntry;
      end if;
      Procs:=NewEntry;
   end Add;
   ---------------------------------------------------------------------------

   procedure Remove
     (Proc : ProcAccess) is
      Entr : ProcEntryAccess:=Procs;
   begin
      while Entr/=null loop
         if Entr.Proc=Proc then
            if Entr.Next/=null then
               Entr.Next.Last:=Entr.Last;
            end if;
            if Entr.Last/=null then
               Entr.Last.Next:=Entr.Next;
            else
               Procs:=Entr.Next;
            end if;
            Free(Entr);
            return;
         end if;
         Entr:=Entr.Next;
      end loop;
   end Remove;
   ---------------------------------------------------------------------------

end ProcessLoop;
