# fortran #

This folder contains the following source code files for StateMod:

| **File** | **Description** |
| -- | -- |
| `*.for` | Fortran source files. |
| `*.inc` | Include files, mainly the `common.inc` file with shared common blocks. |
| `makefile` | makefile to control compile/build, initially the Windows version but should work on Linux. |
| `count-warnings.bash` | Run after `make -k statemod 2>&1 | tee statemod-compile.log` to list counts of different warning types, useful when cleaning up code. |

The following Lahey compiler files are included for historical reasons and may be removed in the future:

| **File** | **Description** |
| -- | -- |
| `AM.bat` | Batch file that used with Lahey compiler. |
| `AUTOMAKE.RSP` | Legacy file used with Lahey compiler. |
| `automake.fig` | Legacy file used with Lahey compiler. |

## Compiling with `gfortran` on Command Line ##

The following is a summary of how to compile.
See the full developer documentation for more information.

### Linux ###

1. Open a Linux terminal window.
2. Change to this folder.
3. Run `make help` to see targets.
4. For a full compile, run `make veryclean` and then `make statemod`.
5. Then copy the executable to a dataset's `StateMod` folder and run.

To check warnings, especially if `FFLAGS` contains `-Wall`, run:

```
make -k statemod 2>&1 | tee statemod-compile.log
./count-warnings.bash
```

### Windows ###

1. Open a MSys2 MinGW window, either 64-bit or 32-bit.
2. Change to this folder.
3. Run `make help` to see targets.
4. For a full compile, run `make veryclean` and then `make statemod`.
5. Then copy the executable to a dataset's `StateMod` folder and run.

To check warnings, especially if `FFLAGS` contains `-Wall`, run:

```
make -k statemod 2>&1 | tee statemod-compile.log
./count-warnings.bash
```

## Compiling with Lahey on Command Line ##

Support for compiling StateMod with Lahey 95 compiler has been retained
to allow comparing Lahey and `gfortran` versions.
To compile with Lahey:

1. Open a Windows command prompt shell.
1. It is assumed that the Lahey compiler is in the `PATH`.
2. Change to this folder.
3. Run `AM` (batch file).
This batch file will ignore Linux and `gfortran` specific files.
