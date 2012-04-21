@ECHO OFF

 IF [%1]==[] GOTO ALL

 IF %1==all GOTO ALL

 IF %1==mpi (
   CALL makempi.bat
   GOTO END
 )

 ECHO *************************************************
 ECHO Valid arguments are:
 ECHO  mpi : to create mpiconstants.ads
 ECHO if no argument are passed, everything is processed
 ECHO *************************************************
 GOTO END

:ALL
 CALL makempi.bat

:END