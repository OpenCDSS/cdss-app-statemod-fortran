rem
rem c1.bat; it compiles all files for crsm
REM **
REM ** IT COMPILES ONE FILE USING THE   ** LAHEY ** COMPILER
rem ** note the /H gives us a *.lst file
rem ** note the /R allows local variables to be remembered
REM **
REM **

erase c:\w\statem\obj\%1.obj
erase statem.exe

F77L3   %1    /B /H /I /L /R /Q1 /NO /NS
erase *.map
