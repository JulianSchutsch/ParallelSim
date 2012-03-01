package SimCommon is

   type NetworkIDString is new String(1..24);

   NetworkContentClientID :
   constant NetworkIDString:="ParallelSimContentClient";

   NetworkContentServerID :
   constant NetworkIDString:="ParallelSimContentServer";

end SimCommon;
