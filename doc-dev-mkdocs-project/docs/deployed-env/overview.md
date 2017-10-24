# Deployed Environment / Overview #

This documentation discusses how StateMod should be deployed into an operational environment.
It is important to keep this target in sight as the end result of software development.
However, software development will occur using a version-controlled copy of files that is different than the deployed environment
(see [New Developer Setup](../dev-new/overview) documentation for how to set up a new developer environment).
Files from the developer environment can be installed to the deployed environment for local testing,
and StateMod users that are not developers will only be working in the deployed environment.

This documentation contains the following sections:

* [Location of Installed StateMod Software](#location-of-installed-statemod-software)
* [Relationship of StateMod Executable with StateMod GUI](#relationship-of-statemod-executable-with-statemod-gui)
* [StateMod 32 and 64 Bit Executable Considerations](#statemod-32-and-64-bit-executable-considerations)

---------------

## Location of Installed StateMod Software ##

The legacy approach for distributing the StateMod executable has been... **TODO smalers 2017-10-24 need to confirm**

**TODO smalers 2016-12-31 Need to work with WWG to confirm where StateMod should be installed going forward.
Perhaps should have a default location under C:\CDSS but also recommend copying to dataset folder to ensure compatibility.
Other issues to be considered include the following:**

* Should the software install into a versioned folder like CDSS TSTool, Python, and other software?
For example:  `C:\CDSS\StateMod-13.03\bin\statemod.exe`.
This would be more flexible when new features are added.
The downside is that it would complicate simple use where "statemod" is entered in a command window,
although the latest install could always update the `PATH` environment variable.
An intelligent StateMod runner script could be developed (similar to Python `py` program)
to look for StateMod in normal locations.
This program could be installed in a common location such as `C:\CDSS\bin`.
* Should a version number be included in the StateMod executable filename?
This is less of an issue if the StateMod install folder is versioned.
`statemod -v` can be run to print the version when in doubt.

## Relationship of StateMod Executable with StateMod GUI ##

The legacy approach for distributing the StateMod executable has been to package the `statemod.exe` file
with the StateMod GUI.  For examaple, see the [CDSS StateMod Download](http://cdss.state.co.us/software/Pages/StateMod.aspx).

**TODO smalers 2016-12-31 Need to decide how the StateMod executable should continue to be packaged with the GUI,
and perhaps also be distributed separately, especially Linux versions.
Also, the StateMod GUI is out of date and needs updated.  Need to prioritize within the OpenCDSS effort.**

## StateMod 32 and 64 Bit Executable Considerations ##

**TODO smalers 2017-10-24 need to update this section - content was copied from StateCU **

The StateMod software is a Fortran program that is compiled to a 32-bit static executable using the `gfortran` compiler.
The 32-bit Windows executable will run on 64-bit Windows 7 and 10 computers similar to other 32-bit software.
Although creating a 64-bit executable may be desirable or necessary in the future, it is currently not the focus of development,
and will require an evaluation of code memory logic and binary output file structure.
See the following resources to understand whether a program has been compiled as a 32-bit or 64-bit executable:

* [10 Ways to Determine if Application is Compiled for 32-bit or 64-bit](https://www.raymond.cc/blog/determine-application-compiled-32-64-bit/)

Based on the above, a useful utility to examine executable properties is the [7zip](http://www.7-zip.org/download.html) software.
Once installed, 7Zip can be used to examine the `statemod.exe` file as follows (the following uses an "el" not "one" 7zip command).
The `CPU = x86` indicates 32-bit, corresponding to i386 computer chips.  A 64-bit executable has `CPU = x64`.

```text
> "C:\Program Files\7-Zip\7zexe" l statemod.exe

7-Zip [64] 16.04 : Copyright (c) 1999-2016 Igor Pavlov : 2016-10-04

Scanning the drive for archives:
1 file, 4587318 bytes (4480 KiB)

Listing archive: statemod.exe

--
Path = statemod.exe
Type = PE
Physical Size = 4587318
CPU = x86
Characteristics = Executable 32-bit NoRelocs NoLineNums
Created = 2017-01-01 03:14:44
Headers Size = 1024
Checksum = 4635747
Image Size = 596111360
Section Alignment = 4096
File Alignment = 512
Code Size = 2475520
Initialized Data Size = 3134976
Uninitialized Data Size = 591632896
Linker Version = 2.25
OS Version = 4.0
Image Version = 1.0
Subsystem Version = 4.0
Subsystem = Windows CUI
Stack Reserve = 2097152
Stack Commit = 4096
Heap Reserve = 1048576
Heap Commit = 4096
Image Base = 4194304

   Date      Time    Attr         Size   Compressed  Name
   ------------------- ----- ------------ ------------  ------------------------
   2017-01-01 03:14:44 .....      2475520      2475520  .text
   2017-01-01 03:14:44 .....         2560         2560  .data
   2017-01-01 03:14:44 .....       613888       613888  .rdata
   2017-01-01 03:14:44 .....        38912        38912  /4
   2017-01-01 03:14:44 .....            0            0  .bss
   2017-01-01 03:14:44 .....         3072         3072  .idata
   2017-01-01 03:14:44 .....          512          512  .CRT
   2017-01-01 03:14:44 .....          512          512  .tls
   2017-01-01 03:14:44 .....         2048         2048  /14
   2017-01-01 03:14:44 .....       765952       765952  /29
   2017-01-01 03:14:44 .....        16896        16896  /41
   2017-01-01 03:14:44 .....       522240       522240  /55
   2017-01-01 03:14:44 .....         1536         1536  /67
   2017-01-01 03:14:44 .....         1024         1024  /78
   2017-01-01 03:14:44 .....       141622       141622  COFF_SYMBOLS
   ------------------- ----- ------------ ------------  ------------------------
   2017-01-01 03:14:44            4586294      4586294  15 files
```

Another option is to use an editor that can edit a binary file. 
For example, use Windows `Notepad`, `Notepad++`, or `vim -b` editors.
Search for the characters `PE` at the top of the file.

* If these characters are followed closely by `L`, then the executable is 32-bit.
* If these characters are followed closely by a `d`, then the executable is 64-bit.
