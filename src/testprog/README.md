# testprog

Programs to test `gfortran` compiler options.
Run `make` to compile and run the various combinations of code and compiler options.

| **Source File** | **Executable Files** | **Description** | **Results** |
| -- | -- | -- | -- |
| `testprog1.for` | `testprog1.exe`, `testprog1-init-local-zero.exe` | Variables are declared locally with subroutines called to modify variables, with no initialization.  The `-finit-local-zero` option is used to create a version with variables initialized to zero. | Variables that are not explicitly initialized may be set to random values.  Using `-finit-local-zero` assigns all variables to zero. |
| `testprog2.for` | `testprog2.exe`, `testprog2-init-local-zero.exe` | Same as `testprog1.for` except that variables are in a common block.  The `-finit-local-zero` option is used to create a version with variables initialized to zero. | Variables are initialized to zero whether or not `-finit-local-zero` is used because apparently using in a common block requires initial values. |
| `testprog3.for` | `testprog3.exe`, `testprog3-init-local-zero.exe`, `testprog3-no-automatic-init-local-zero.exe` | Same as `testprog1.for` except that `set_using_local` subroutine contains local variable that is impacted by compiler options.  The `-finit-local-zero` option is used to create a version with variables initialized to zero and increment only results in output of 1.  If `-no-automatic` is also used, the values in `set_using_local` are retained between calls and increment. | Using `-fno-automatic` and `-finit-local-zero` together means that values are initialized to zero once and then the saved values persist between calls. |
