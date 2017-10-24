# Development Tasks / FAQ for MinGW #

The MinGW environment is intended as a self-contained developer environment for C, Fortran, and other languages.
It is not intended as an operational environment (Cygwin is recommended for that).
MinGW is installed with a number of Unix/Linux system tools to facilitate development work, referred to as MSYS.

Answers to the following questions are provided in this documentation:

* [How do I access MinGW/MSYS programs from the Windows Command Shell?](#how-do-i-access-mingwmsys-programs-from-the-windows-command-shell)

------------

## How do I access MinGW/MSYS programs from the Windows Command Shell? ##

As part of the MinGW setup, it is recommended that a batch file is created to set environment variables,
in particular the `PATH`, in order to find MinGW and MSYS software executables.
A default script is available in the repository:  `~/cdss-dev/StateCU/git-repos/cdss-app-statecu-fortran/build-util/mingw/setup-mingw-env.bat`.
If this script is run, then opening a Windows Command Shell should show the modified path:

```text
>echo %PATH%

C:\MinGW\bin;C:\MinGW\MSYS\1.0\local\bin;C:\MinGW\MSYS\1.0\bin;C:\Program Files\Python35\Scripts\;C:\Program Files\Python35\;C:\Program Files (x86)\Intel\iCLS Client\;C:\Program Files\Intel\iCLS Client\;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\Program Files (x86)\Intel\Intel(R) Management Engine Components\DAL;C:\Program Files\Intel\Intel(R) Management Engine Components\DAL;C:\Program Files (x86)\Intel\Intel(R) Management Engine Components\IPT;C:\Program Files\Intel\Intel(R) Management Engine Components\IPT;c:\Program Files (x86)\ATI Technologies\ATI.ACE\Core-Static;C:\Program Files\Intel\WiFi\bin\;C:\Program Files\Common Files\Intel\WirelessCommon\;C:\Users\sam\AppData\Local\Microsoft\WindowsApps
```

Then programs like `make` and `gfortran` can be run from the Windows command line.
