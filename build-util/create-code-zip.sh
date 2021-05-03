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
# Include relevant files and ignore dynamic files.
# - don't include Lahey files because Ray Bennett is in control and needs to make those work
# - ignore specific files that are used in development but should not be included (can't seem to use ls -I for this so use grep)
ls -1 fortran/*.inc fortran/*.for fortran/makefile fortran/*.bash fortran/*.sh fortran/*.md | grep -v junk-statem.for > ${listFile}

zipFile="statemod-${statemodVersion}-code.zip"

# Remove the file first so there is no weird merging.
zipFile="statemod-${statemodVersion}-code.zip"
if [ -f "${zipFile}" ]; then
  echo "Removing old zip file:  ${zipFile}"
  rm "${zipFile}"
fi

echo "Running 7zip to create zip file:  ${zipFile}"
"${sevenzip}" a -tzip ${zipFile} @${listFile}
exitStatus=$?
if [ ${exitStatus} -ne 0 ]; then
  echo "Error running 7zip, exit status (${exitStatus})."
  exit 1
fi

echo "Zip file for current code version is:  ${srcMainFolder}/${zipFile}"
echo "Copy to :  ${srcMainFolder}/${zipFile}.txt if necessary to email."
exit 0
