@ECHO OFF
rem AUTOMAKE looks for source files matching FILES in automake.fig.
rem It then creates the temporary file AMTEMP.BAT with a list
rem of source files to compile (because they changed).
rem It also creates AUTOMAKE.RSP with a list of all object
rem files to be linked into the final executable.
AUTOMAKE type=%1 %2 %3 %4 %5 %6 %7 %8 %9
@IF ERRORLEVEL 1 GOTO delete

rem The AMTEMP.BAT file will contain extra files like *linux*
rem and *gfortran* files not used by the Lahey compile.
rem Remove the files from AMTEMP.BAT and save to an updated version.
rem AUTOMAKE seems to always output uppercase filenames,
rem even though source are lowercase, so use uppercase in filter below.
rem This will remove the comment and compile line but leave
rem lines checking error code, which is OK.
rem See:  https://stackoverflow.com/questions/418916/delete-certain-lines-in-a-txt-file-via-a-batch-file
echo Removing Linux and gfortran files from AMTEMP.BAT into AMTEMP2.BAT
type AMTEMP.BAT | findstr /v GETPATH_LINUX.FOR | findstr /v PUTPATH_LINUX.FOR | findstr /v DATTIM_GFORTRAN.FOR | findstr /v PARSE_GFORTRAN.FOR > AMTEMP2.BAT

rem Now copy the modified AMTEMP2.bat version back to the original AMTEMP.BAT.
rem Do this to make sure the above does not cause an issue with the file stepping on itself.
rem Do a copy so that AMTEMP2.BAT can be reviewed for troubleshooting.
echo Copying AMTEMP2.BAT to AMTEMP.BAT
copy AMTEMP2.BAT AMTEMP.BAT

rem Do the same thing for AUTOMAKE.RSP as was done for AMTEMP.BAT.
echo Removing Linux and gfortran files from AUTOMAKE.RSP into AUTOMAKE2.RSP
type AUTOMAKE.RSP | findstr /v GETPATH_LINUX.OBJ | findstr /v PUTPATH_LINUX.OBJ | findstr /v DATTIM_GFORTRAN.OBJ | findstr /v PARSE_GFORTRAN.OBJ > AUTOMAKE2.RSP
echo Copying AUTOMAKE2.RSP to AUTOMAKE.RSP
copy AUTOMAKE2.RSP AUTOMAKE.RSP

rem Run AMTEMP.BAT to compile existing .OBJ and newly compiled .OBJ
rem into the final executable.
echo Calling AMTEMP to compile StateMod
call AMTEMP

rem Delete the AMTEMP.BAT file, presumably because it may be in error.
rem This is commented out to allow for troubleshooting but maybe should be uncommented?
:delete
rem DEL AMTEMP.BAT

rem copy to Standard statemod directory so I dont have to revise all the *.bat commands
rem TODO smalers 2021-03-11 Need to wrap the following in if to make only work for Ray Bennett,
rem for examaple to check %USER%
rem copy statemod.exe c:\w\statem\forwell
rem copy statemod.exe c:\w\statem\forwell\StateMod_160042.exe
rem copy StateMod.exe StateMod_160042.exe
