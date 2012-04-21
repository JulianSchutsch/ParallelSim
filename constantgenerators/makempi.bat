@ECHO OFF
 ECHO *** Compiling mpi.c ***
 IF DEFINED C_INCLUDE_PATH (
  gcc -Wall -c mpi.c -I%C_INCLUDE_PATH%
 ) ELSE (
  gcc -Wall -c mpi.c -lmpi
 )
 IF %ERRORLEVEL% NEQ 0 GOTO COMPILEERROR
 ECHO *** Linking mpi.exe ***
 gcc mpi.o -o mpi.exe -L"C:/Program Files (x86)/MPICH2/lib" -static -lmpi
 IF %ERRORLEVEL% NEQ 0 GOTO LINKERROR
 ECHO *** Generating mpi-constants ***
 mpi.exe
 ECHO *** Done ***
 MOVE /Y mpiconstants.ads ..\src\distributedsystems
 GOTO END
:COMPILEERROR
 ECHO ################################################################################
 ECHO # Compilation of mpi.c failed                                                  #
 ECHO #                                                                              #
 ECHO # Please ensure presence of mpi.h by installing an MPI implementation and      #
 ECHO # development files.                                                           #
 ECHO # If the mpi.h is not in a standard path, please specify search path in the    #
 ECHO # C_INCLUDE_PATH environment variable.                                         #
 ECHO ################################################################################
 GOTO END
:LINKERROR
 ECHO ################################################################################
 ECHO # Linking of mpi.exe failed                                                    #
 ECHO ################################################################################
:END