@echo off
rem Setup the MinGW environment variables
rem See:  http://www.mingw.org/wiki/Getting_Started


rem Check whether this script needs to be run.
rem If MINGW_SETUP=YES, then it was already run so go to the end and exit

if "%MinGW_SETUP%"=="YES" (
	echo MinGW_SETUP=YES so not doing setup again
	goto end
)

rem Update PATH to find the MinGW bin folder and also the MSYS folders (Unix utilities).
echo.
echo Configuring this Windows command prompt window to compile StateMod with gfortran.
echo Run setup-mingw-env.bat in each command prompt window that is opened for compiling.
echo Setting PATH to include MinGW software locations at front of PATH:
echo.
set PATH=C:\MinGW\bin;C:\MinGW\MSYS\1.0\local\bin;C:\MinGW\MSYS\1.0\bin;%PATH%
echo %PATH%

rem Set an environment variable to know that the environment was set up
rem so that rerunning the batch file does not add to the path
set MinGW_SETUP=YES

rem Set the title of the window
title MINGW configured to compile using gfortran

rem If here can end the script
:end
