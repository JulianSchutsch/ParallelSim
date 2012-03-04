package SimCommon is

   type NetworkIDString is new String(1..24);

   NetworkControlClientID :
   constant NetworkIDString:="ParallelSimControlClient";

   NetworkControlServerID :
   constant NetworkIDString:="ParallelSimControlServer";

end SimCommon;
