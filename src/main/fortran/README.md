# cdss-app-statemod-fortran/src/main/fortran #

This folder contains the source code files for StateMod:

* `*.for` - Fortran source files
* `*.inc` - include files, mainly the `common.inc` file with shared common blocks
* makefile - makefile to control compile/build, initially the Windows version but Linux rules will be added

The following files are included for historical reasons and may be removed in the future:

* `AM.bat` - batch file that used with Lahey compiler
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

## Compiling with Lahey on Command Line ##

Support for compiling StateMod with Lahey 95 compiler has been retained
to allow comparing Lahey and `gfortran` versions.
To compile with Lahey:

1. Open a Windows command prompt shell.
1. It is assumed that the Lahey compiler is in the `PATH`.
2. Change to this folder.
3. Run `AM` (batch file).
This batch file will ignore Linux and `gfortran` specific files.
