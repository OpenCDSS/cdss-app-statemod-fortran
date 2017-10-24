# Development Tasks / Deploying #

Deploying the software consists of compiling StateMod into an executable and packaging into an installer that can be distributed.

**TODO smalers 2017-01-02 Need to coordinate with Andy Moore and Erin Wilson on how StateMod should be deployed... with the GUI, separate, etc.
Also need to evaluate whether to deploy from the Git Project page and/or CDSS website.**

This documentation contains the following sections:

* [Build Checklist](#build-checklist)
* [Creating StateMod Installer](#creating-statemod-installer)
* [Releasing StateMod](#releasing-statemod)

---------------------

## Build Checklist ##

Currently there is not a need for StateMod to be built in a continuous integration process because the development team is small.
Consequently the build process is done by the "buildmaster".

The full build process checklist is as follows:

1. **TODO smalers 2017-01-10 need to discuss software version number and release notes.**
2. **TODO smalers 2017-01-10 need to discuss cleaning up branches into master so there are no loose ends, tagging release, etc..**
3. Run `make clean`.
4. Run `make statemod`.
5. **TODO smalers 2017-01-10 need make targets for creating installer, etc.... does this depend on a StateMod GUI release? link to below.**
6. **TODO smalers 2017-01-10 need to discuss deploying installer to GitHub pages, CDSS website, etc. link to below.**

## Creating StateMod Installer ##

**TODO smalers 2017-01-10 include here details of creating the installer**

## Releasing StateMod ##

**TODO smalers 2017-01-10 include here details of public release**
