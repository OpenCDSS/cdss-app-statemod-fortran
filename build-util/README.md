# build-util #

This folder contains utility scripts and Windows batch files that are used in the build process.

| **File** | **Description** |
| -- | -- |
| `copy-to-co-dnr-gcp.bash` | Create and copy the StateMod zip file installer to Google Cloud platform, for public access. |
| `create-gcp-statecu-index.bash` | Create an `index.html` file on GCP that lists the installer downloads and associated documentation. |
| `create-code-zip.sh` | Creates a zip file of the source code folder, used to transfer current code to Ray Bennett. |
| `eclipse/run-eclipse-statemod-mingw.bat` | Windows batch file to start proper version of Eclipse with proper Java version, used if Photran is used for development. |
| `git-check-statemod.md` | Script to indicate status of all StateMod repositories. |
| `git-clone-all-statemod.md` | Script to clone all StateMod repositories in new developer environment (must first clone the main `cdss-app-statemod-fortran` repository to gain access to this script). |
| `git-util` | General version of Git utilities called by specific `git*` scripts. |
| `mingw/setup-mingw-env.bat` | Windows batch file to configure MINGW environment for compiling with `gfortran` - **no longer needed with MSys2 environment**. |
| `product-repo-list.txt` | Repository list used by `git-check.sh`. |
