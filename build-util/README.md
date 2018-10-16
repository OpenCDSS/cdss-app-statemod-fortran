# cdss-app-statemod-fortran/build-util #

This folder contains utility scripts (or batch files if Windows) that are used in the build process:

* `eclipse/run-eclipse-statemod-mingw.bat` - batch file to start proper version of Eclipse with proper Java version
* `git-check-statemod.md` - script to indicate status of all StateMod repositories
* `git-clone-all-statemod.md` - script to clone all StateMod repositories in new developer environment
(must first clone the main `cdss-app-statemod-fortran` repository to gain access to this script
* `git-util` - general version of Git utilities called by specific `git*` scripts
* `mingw/setup-mingw-env.bat` - batch file to configure MINGW environment for compiling with `gfortran`
* `product-repo-list.txt` - repository list used by `git-check.sh`
