# Development Tasks / Compiling #

StateMod code is compiled using a "makefile", which defines rules for detecting when a file needs to be recompiled,
based on code dependencies.
The code can be compiled from command line or from the Eclipse IDE.
The Eclipse IDE provides benefits during development but it may be necessary or useful to compile on the command line,
for example, if automating the build process.

This documentation contains the following sections:

* [Compile StateMod on Command Line](#compile-statemod-on-command-line)
* [Compile StateMod in Eclipse](#compile-statemod-in-eclipse)

----------------------

## Compile StateMod on Command Line ##

### ![Linux](../../images/linux-32.png) Linux ###

### ![Windows](../../images/windows-32.ico) Windows - MinGW ###

To compile StateMod on the command line it is first necessary to configure the environment to run the compiler.
Open a Windows command prompt window and change to the folder where the setup script exists.
Then run the `setup-mingw-env.bat` batch file to configure the MinGW environment (note that setting up the environment in the window only needs
to be done once after the window is opened).

```
> C:
> cd \Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util\mingw
> setup-mingw-env.bat
```

Then change to the code location and run the makefile:

```
> C:
> cd \Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\src\main\fortran
> make clean
> make statemod
```

The executable `statemod.exe` is created in the same folder and can be run with model input (in a test folder separate from the code).

## Compile StateMod in Eclipse ##

### ![Linux](../../images/linux-32.png) Linux ###

**TODO smalers 2017-10-24 need to complete this section**

### ![Windows](../../images/windows-32.ico) Windows - MinGW ###

To compile StateMod in Eclipse, start Eclipse with the run script `run-eclipse-statemod-mingw.bat` as shown below.
This script automatically runs the MinGW setup script described in the previous section,
which will configure the compiler environment if necessary.


```
> C:
> cd \Users\user\cdss-dev\StateMod\git-repos\cdss-app-statemod-fortran\build-util\eclipse
> run-eclipse-statemod-mingw.bat
```

Then right-click in the ***Project Explorer*** area and select ***Make / Targets***.  Then select ***Build...***.  Then select a target and press the ***Build*** button.

Review the output in the ***Console*** area to see if any errors occurred.
