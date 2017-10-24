# Initial Project Setup / Eclipse Run Script #

It is helpful to run Eclipse with a script to ensure that proper versions of Eclipse and Java are used.
Note that the script described below
was initially created before setting up the Eclipse workspace and thereafter can be run to start Eclipse.
The script was created outside of the repository and was then moved to the repository location described below.

The following sections are included in this documentation:

* [Prerequisites](#prerequisites)
* [Default Eclipse Run Script](#default-eclipse-run-script) - run script provided in repository for standard project configuration
	+ ![Linux](../../images/linux-32.png) [Linux](#linux)
	+ ![Windows](../../images/windows-32.ico) [Windows](#windows)
* [Developer-specific Run Script](#developer-specific-run-script) - modify the default for a specific user
* [Next Step](#next-step)

--------------

## Prerequisites ##

This step requires that Eclipse/Photran was previously installed as per the [Developer Environment / Eclipse and Photran](../dev-env/eclipse/) documentation.

## Default Eclipse Run Script ##

### ![Linux](../../images/linux-32.png) Linux ###

**TODO smalers 2016-12-31 need to complete this - the initial focus has been on Windows.**

### ![Windows](../../images/windows-32.ico) Windows ###

The following batch file was created to run Eclipse and is suitable if the project
is configured as per this documentation.
It is recommended that developers use the script as is if possible to run from a Windows command shell that has
been initialized with the proper MinGW environment variables.

```text
C:\Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util\eclipse\run-eclipse-statemod.bat
```

A snapshot of the script contents is as follows ([see the repository for current version](https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran/blob/master/build-util/eclipse/run-eclipse-statemod-mingw.bat)):

```bat
rem Run Eclipse with Java 8, to make sure the right versions are used
rem This assumes that Java 8 and Eclipse have been installed as per the StateMod developer documentation.

rem Specify the VM to use and the maximum memory
"C:\Program Files\Eclipse\eclipse-parallel-mars-64\eclipse" -vm "C:\Program Files\Java\jre8\bin\java" -vmargs -Xmx700M

```

Note that the command line parameters passed to Eclipse are an alternative to changing the `eclipse.ini` file 
distributed with the Eclipse software.
If the above is not suitable, it is recommended that the above script is copied as a developer-specific version as described in the next section.

## Developer-specific Run Script ##

If the above script is not appropriate for the specific developer, make a copy and modify.
For example, save in a `cdss-dev\StateMod\build-util` folder.

## Next Step ##

Use the run script to start Eclipse.  The next step is to set up an initial Eclipse workspace.
