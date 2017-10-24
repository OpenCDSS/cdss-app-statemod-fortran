# Development Environment / Overview #

This Development Environment documentation is intended to be used as a reference by the developer that
sets up the project for the first time or initializes a new development environment to contribute to the project.
Specific sections are referenced by the [Initial Project Setup](../project-init/overview/),
[Deployed Environment](../deployed-env/overview/), and
[New Developer](../dev-new/overview/) sections.

This documentation includes the following sections:

* [Development Environment Software Requirements](#development-environment-software-requirements)
* [Software Install Location Considerations](#software-install-location-considerations)
* [Portability Considerations](#portability-considerations)

-----

## Development Environment Software Requirements ##

The following software are needed in the StateMod development environment and should be installed before doing [Initial Project Setup](../project-init/overview/),
although the Initial Project Setup documentation generally indicates prerequisites for software that needs to be installed.
Steps can be skipped if they have been completed previously as part of operating system setup or on other software development projects.
Some additional software may be installed in the development files and is described in project initialization.

The development environment as described provides a full technology stack for development.
It is possible to use a subset of the components (text editor and compiler rather than Eclipse/Photran IDE, for example);
however, the full stack is described in order to provide a fully-integrated development environment.
Software developers need to invest in training to be competent with the various tools,
although this documentation is intended to help facilitate development by providing useful information.

The following software are required for StateMod development:

1. [Machine](machine) - minimal GNU for Windows (MinGW), Cygwin, or Linux virtual machine (VM), to support `gfortran` compiler
(**MinGW is the initial focus because it is the standard environment for gfortran on Windows**)
2. [Git](git) - needed to perform command line version control operations
3. [Python and pip](python) - needed by MkDocs and pytest, and useful general tool (**skip if not editing MkDocs documentation and not creating automated tests with Python**)
4. [pytest](pytest) - an option being evaluated for automated testing (**skip if not creating automated tests with Python**)
5. [MkDocs](mkdocs) - MkDocs is used for developer and user documentation static websites, including this documentation (**skip if not editing MkDocs documentation**)
6. [Java 8](java8) - used to run Eclipse, and can be used to write utility programs (**skip if using a text editor rather than Eclipse/Photran**)
7. [gfortran](gfortran) - compiler for the StateMod Fortran software
8. [Eclipse/Photran](eclipse) - IDE used for interactive Fortran software development (**skip if using a text editor rather than Eclipse/Photran**)
9. [Text Editor](editor) - editor used to edit code and other files, useful in any case
10. [Doxygen](doxygen) - documentation tool (**skip if not creating graphs of code calls**)
11. [KDiff3](kdiff3) - tool for comparing files (**skip if not comparing files or have equivalent tool**)

## Software Install Location Considerations ##

The software development environment must be appropriately configured to effectively develop StateMod and support collaboration.
Development environment setup is a relatively major effort before any code can be written
and is typically done the first time by someone that has a good grasp of the technologies.
Failing to understand how to set up the development environment will likely lead to wasted time
and possibly back-tracking on previous software development work.
Using a consistent development environment for all developers ensures that documentation is applicable and troubleshooting is consistent.
Therefore, the software installation locations described in this documentation are recommended to avoid issues.

See the discussion of [Initial Project Setup / Development Folder Structure](../project-init/overview#development-folder-structure)
for a folder structure that is assumed in this documentation.
It is important for software developers to understand the software tools and configuration so that they can troubleshoot configuration issues.

## Portability Considerations ##

The StateMod development environment is intended to be as portable as possible,
in order to allow multiple software developers to contribute to the software on multiple operating systems.
Portable configuration minimizes day-to-day conflicts in developer environment that result in lost productivity and software quality.
Portability considerations are discussed where appropriate.
