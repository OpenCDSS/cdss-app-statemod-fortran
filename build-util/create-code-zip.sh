#!/bin/sh

# Create a zip file containing code, used to transfer files to Ray Bennett.

# Main entry point for script

# Get the location where this script is located since it may have been run from any folder
scriptFolder=`cd $(dirname "$0") && pwd`
repoFolder=$(dirname "$scriptFolder")
srcFolder="$repoFolder/src"
srcMainFolder="${srcFolder}/main"

# Change to 'main' folder, which is above 'fortran'.
echo "Changing to code folder:  ${srcMainFolder}"
cd ${srcMainFolder}

# Get the StateMod version
statemFile="fortran/statem.for"
statemodVersion=$(cat ${statemFile} | grep 'ver =' | grep -v 'xx' | cut -d '=' -f 2 | sed "s/'//g" | tr -d ' ')
echo "StateMod version determined to be: ${statemodVersion}"

# Zip up all files in the folder that have file extensions of interest
# - use 7zip command line (must be installed in normal location)
# - zip with the folder so that there is less chance to clobber existing files
# - use a list file to control what files are included
# - the zip file includes the StateMod version from statem.for file
sevenzip='/C/Program Files/7-Zip/7z.exe'
listFile="/tmp/statemod-code-list.txt"
ls -1 fortran/*.inc fortran/*.for fortran/makefile > ${listFile}

zipFile="statemod-${statemodVersion}-code.zip"
"${sevenzip}" a -tzip ${zipFile} @${listFile}
