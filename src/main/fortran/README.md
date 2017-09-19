# cdss-app-statemod-fortran/src/main/fortran #

This folder contains the source code files for StateMod:

* *.for - Fortran source files
* *.inc - include files, mainly the `common.inc` file with shared common blocks
* makefile - makefile to control compile/build, initially the Windows version but Linux rules will be added

The following files are included for historical reasons and may be removed in the future:

* AUTOMAKE.RSP - legacy file used with Lahey compiler
* automake.fig - legacy file used with Lahey compiler

## Compiling with gfortran on Command Line ##

The following is a summary of how to compile.
See the full developer documentation for more information.

### Linux ###

Add later...

### Windows ###

1. Open a Windows command prompt shell.
2. Setup the MinGW environment by running the `build-util\mingw\setup-mingw-env.bat` batch file.
3. Change to this folder.
4. Run `make`.
