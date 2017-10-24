# Development Tasks / Run Eclipse #

The Eclipse/Photran IDE is useful for interactive code development, debugging, and testing.
Currently, the Eclipse/Photran IDE is the only IDE that is described in this documentation.
It is not necessary to use Eclipse/Photran or another IDE to compile the software,
but the IDE does provide an integrated interface to the code and development tasks.

This documentation contains the following sections:

* [Configure Environment](#configure-environment)
* [Start Eclipse](#start-eclipse)

----------

## Configure Environment ##

The development environment must be configured properly in order for Eclipse to find the compilers and other software.

### ![Linux](../../images/linux-32.png) Linux ###

**TODO smalers 2016-12-31 fill in when there is time to test on Linux**

### ![Windows](../../images/windows-32.ico) Windows ###

**This step is included by default if the Eclipse run script is used, as described in the next section.**

Assuming that the development environment is consistent with this documentation, run the script from a Windows command shell:
`C:\Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util\mingw\setup-mingw-env.bat`.

## Start Eclipse ##

Assuming that the development environment is consistent with this documentation, run the script from a Windows command shell:
`C:\Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util\eclipse\run-eclipse-statemod-mingw.bat`.

After Eclipse starts, pick the StateMod workspace: `C:\Users\user\cdss-dev\StateMod\eclipse-workspace`.
