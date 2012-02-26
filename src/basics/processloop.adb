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


end ProcessLoop;
