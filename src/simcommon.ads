package SimCommon is

   type NetworkIDString is new String(1..19);

   NetworkElementID :
   constant NetworkIDString:="ParallelSimElement ";

   NetworkControlID :
   constant NetworkIDString:="ParallelSimControl ";

   NetworkAdminID :
   constant NetworkIDString:="ParallelSimAdmin   ";

   NetworkEntranceID :
   constant NetworkIDString:="ParallelSimEntrance";

   NetworkClientID :
   constant NetworkIDString:="ParallelSimClient  ";


end SimCommon;
