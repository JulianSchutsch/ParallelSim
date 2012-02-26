package ProcessLoop is

   type ProcAccess is
     access procedure;

   procedure Process;

   procedure Add
     (Proc : ProcAccess);

   procedure Remove
     (Proc : ProcAccess);

end ProcessLoop;
