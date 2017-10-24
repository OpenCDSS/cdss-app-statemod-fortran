# Development Tasks / FAQ for Cygwin #

The Cygwin environment is intended as a full-featured Linux-like environment running within Windows,
with executable programs that are compiled to be compatible with Windows.
Whereas MinGW focuses on providing a minimal environment for development tools,
Cygwin provides an extensive environment with many software packages.

Answers to the following questions are provided in this documentation:

* [How do I open a Cygwin Terminal?](#how-do-i-open a Cygwin Terminal?)
* [How do I navigate Windows folders in Cygwin?](#how-do-i-navigate-windows-folders-in-cygwin)
* [How do I access Cygwin programs from the Windows Command Shell?](#how-do-i-access-cygwin-programs-from-the-windows-command-shell)

----------------

## How do I open a Cygwin Terminal? ##

Cygwin, when installed, typically creates a desktop shortcut to create a Cygwin terminal.
Running the program opens a terminal window that runs Bash and provides command-line access.

## How do I navigate Windows folders in Cygwin? ##

The Cygwin terminal behaves like a typical command line interface.
Because Cygwin is Linux-like, folders are separated with forward slash `/`.  Cygwin uses a root level folder `/cygdrive/C` (rather than `C:\`).
To access native Windows files, prefix `/cygdrive/C` (or other drive letter) before folder names and then navigate to subfolders.

## How do I access Cygwin programs from the Windows Command Shell? ##

The Cygdrive shell automatically uses the Windows `PATH` as well as prefixes some Cygwin folders.
Consequently, most Windows programs are available from the Cygwin command shell, including batch files.
Use the `which` program to find an executable program from the `PATH`, for example:

```bash
$ which date

/usr/bin/date
```

Programs that have the same name in Cygwin and Windows will be found depending on which one is first in the `PATH`.
