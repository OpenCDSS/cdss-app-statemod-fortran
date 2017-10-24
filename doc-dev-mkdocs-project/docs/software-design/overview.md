# Software Design / Overview

**TODO smalers 2016-12-31 This is a very rough outline and needs work to educate developers about the StateMod code organization.**

The StateMod Fortran code is divided into separate files with .for extension.
Each subroutine is in a separate file and the name of the file matches the subroutine (is this globally true? a grep on code seemed to indicate so).

Need to describe code modules at a high level:

* Main entry point (see: [`statem.for`](https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran/blob/master/src/main/fortran/statem.for))
* Code organization:  include files, parameter data, etc.
* Input
* Initialization
	+ Global data
	+ Main program data
* Calculations
* Output
* Data checks
* Logging
* Clean-up
* Integration with StateMod GUI

## StateMod Code API

**TODO smalers 2017-10-24 need to update to use StateMod Doxygen output - this content was copied from StateCU documentation**

The StateMod code has been processed with Doxygen software to produce HTML documentation.
Additional code formatting will occur to enable more complete documentation.

* [StateMod 13.10 code documentation](http://software.openwaterfoundation.org/cdss/statemod/13.10/doc-api/index.html)

## Fortran Conventions

StateMod has been in existence for many years and the code as evolved through different versions of the Fortran language.
The following is a summary of current conventions:

* Generally adheres to Fortran 77/?? conventions - does not utilize newer Fortran ?? features (need to explain).
* List naming conventions
* Other conventions

## Data Passing

As per typical Fortran conventions, some data values are passed to subroutines and functions via parameters
and other data are shared between subroutines via common blocks.

Need to list common blocks and explain purpose.

Need to explain global/static data, dimensions, etc.
