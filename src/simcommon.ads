package SimCommon is

   type NetworkIDString is new String(1..24);

   NetworkControlServerID :
   constant NetworkIDString:="ParallelSimControlServer";

   NetworkControlClientID :
   constant NetworkIDString:="ParallelSimControlClient";

   NetworkAdminServerID :
   constant NetworkIDString:="ParallelSimAdminServer  ";

   NetworkAdminClientID :
   constant NetworkIDString:="ParallelSimAdminClient  ";

end SimCommon;
