# Initial Project Setup / Build Utility Scripts #

This documentation provides a reference for development environment and build utility scripts.
The default versions of these scripts have been saved in the repository to allow a new developer to directly use the scripts,
assuming that they have set up the development environment as per this documentation.
Copies of the scripts can be modified for specific developers and saved to a location specific to the user,
for example `~/cdss-dev/StateMod/build-util` (not in the repository).
Or, utility scripts can be updated based on experience of developers in order to be generally useful.
Additional scripts will be added as the need is identified.

This documentation includes the following sections:

* [Build Utility Script Location](#build-utility-script-location)
* [Script to Configure MinGW Environment - `setup-mingw-env`](#script-to-configure-mingw-environment-setup-mingw-env)
* [Script to Run Eclipse - `run-eclipse-statemod`](#script-to-run-eclipse-run-eclipse-statemod-mingw)

----------

## Build Utility Script Location ##

The build utilities are located in the `build-util` folder in the repository.

For ![Linux](../../images/linux-32.png) Linux:  `~/cdss-dev/StateMod/git-repos/cdss-app-statemod-fortran/build-util`.

For ![Windows](../../images/windows-32.ico) Windows:  `C:\Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util`.

For Windows the folder structure is similar to the following:

```txt
C:/Users/user/
    cdss-dev/
        StateMod/
            git-repos/
                cdss-app-statemod-fortran/
                    build-util/
                        eclipse/
                            run-eclipse-statemod-mingw.bat
                        mingw/
                            setup-mingw-env.bat
```

## Script to Configure MinGW Environment - `setup-mingw-env` ##

### ![Windows](../../images/windows-32.ico) Windows ###

Run this batch file to configure the MinGW environment so that compilers can be found in the command shell and in eclipse.
This batch file is called by other batch files that use the MinGW environment, such as `run-eclipse-statemod-mingw.bat`.

This batch file is described first in [Develompent Environment / Machine](dev-env/machine/) and is located in `build-util/mingw`.

## Script to Run Eclipse - `run-eclipse-statemod-mingw` ##

### ![Windows](../../images/windows-32.ico) Windows ###

Use this batch file to start Eclipse in the MinGW environment.
This batch file calls `setup-mingw-env` before calling Eclipse.

This batch file is described first in [Initial Project Setup / Eclipse Run Script](eclipse-run-script/) and is located in `build-util/eclipse`.
