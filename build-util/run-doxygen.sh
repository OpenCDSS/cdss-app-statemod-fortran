#!/bin/sh

# Run the Doxygen software on source code to auto-generate documentation.

# Name of the main repository name
mainRepoName="cdss-app-statemod-fortran"

# Determine which echo to use, needs to support -e
# - normally built-in shell echo is OK, but on Debian Linux dash is used, and it does not support -e
echo2='echo -e'
testEcho=`echo -e test`
if [ "${testEcho}" = '-e test' ]; then
        # The -e option did not work as intended.
        # -using the normal /bin/echo should work
        # -printf is also an option
        echo2='/bin/echo -e'
        # The following does not seem to work
        #echo2='printf'
fi

# Strings to change colors on output, to make it easier to indicate when actions are needed
# - Colors in Git Bash:  https://stackoverflow.com/questions/21243172/how-to-change-rgb-colors-in-git-bash-for-windows
# - Useful info:  http://webhome.csc.uvic.ca/~sae/seng265/fall04/tips/s265s047-tips/bash-using-colors.html
# - See colors:  https://en.wikipedia.org/wiki/ANSI_escape_code#Unix-like_systems
# - Set the background to black to eensure that white background window will clearly show colors contrasting on black.
# - Yellow "33" in Linux can show as brown, see:  https://unix.stackexchange.com/questions/192660/yellow-appears-as-brown-in-konsole
# - Tried to use RGB but could not get it to work - for now live with "yellow" as it is
actionColor='\e[0;40;33m' # user needs to do something, 40=background black, 33=yellow
okColor='\e[0;40;32m' # status is good, 40=background black, 32=green
endColor='\e[0m' # To switch back to default color

# Get the location where the script is located since it may be run from any folder
#scriptDir=`dirname $0`
scriptDir=`cd $(dirname "$0") && pwd`
mainRepoFolder=`dirname ${scriptDir}`
echo "Main repo folder:  ${mainRepoFolder}"
# If necessary, hard code for standard folder to have a nicer path without ..
# and force consistency among developers.
#mainRepoFolder="$HOME/cdss-dev/StateMod/git-repos/${mainRepoName}"
if [ ! -d "${mainRepoFolder}" ]; then
	echo ""
	echo "${mainRepoName} repository folder does not exist:"
	echo "  ${mainRepoFolder}"
	echo "Exiting."
	echo ""
	exit 1
fi

# Get the software version from the statem.for file code:
#     ver = '15.00.14'
programVersionFile="${mainRepoFolder}/src/main/fortran/statem.for"
if [ ! -f "${programVersionFile}" ]; then
	echo ""
	echo "StateMod version file does not exist:"
	echo "  ${programVersionFile}"
	echo "Exiting."
	echo ""
	exit 1
fi
# Find the matching line with grep command.
# Use head command to get only the first instance (there are 2).
# Cut out the version from the found line.
programVersion=$(cat ${programVersionFile} | grep 'ver =' | grep -v 'xx' | cut -d '=' -f 2 | sed "s/'//g" | tr -d ' ')
if [ -z "${programVersion}" ]; then
	echo ""
	echo "Unable to determine the StateMod version from ${programVersionFile}.  Exiting."
	exit 1
else
	echo ""
	echo "StateMod version detected to be ${programVersion}"
	echo "The version will be used for the Doxygen output."
fi

# In order for the Doxygen configuration to allow relative path,
# change to the Doxygen project folder and then start the software.
doxygenProjectFolder="${mainRepoFolder}/doc-dev-doxygen-project"
if [ ! -d "${doxygenProjectFolder}" ]; then
	echo ""
	echo "The Doxygen project folder does not exist:"
	echo "  ${doxygenProjectFolder}"
	echo "Exiting."
	echo ""
	exit 1
fi
echo ""
echo "Changing to Doxygen project folder: ${doxygenProjectFolder}"
cd "${doxygenProjectFolder}"

# Make sure the Doxygen template file exists
doxyfileTemplate="${doxygenProjectFolder}/Doxyfile-template"
if [ ! -f "${doxyfileTemplate}" ]; then
	echo ""
	echo "The Doxyfile template does not exist:"
	echo "  ${doxyfileTemplate}"
	echo "Exiting."
	echo ""
	exit 1
fi

# If it does not exist, create an output folder for the StateMod version
doxygenWorkFolder="${doxygenProjectFolder}/doxygen-statemod-${programVersion}"
doxygenWorkFolderRelative="doxygen-statemod-${programVersion}"
if [ ! -d "${doxygenWorkFolder}" ]; then
	echo ""
	echo "Creating Doxygen working folder for StateMod version:"
	echo "  ${doxygenWorkFolder}"
	echo ""
	mkdir "${doxygenWorkFolder}"
	if [ "$?" -ne 0 ]; then
		echo ""
		echo "Unable to create Doxygen working folder:"
		echo "  ${doxygenWorkFolder}"
		echo "Exiting."
		exit 1
	fi
fi

# Process the Doxyfile-template into final Doxyfile used by Doxygen
# - replace the software version with program version
# - output to a versioned folder so multiple version-specific Doxygen output exist
doxyfile="${doxygenWorkFolder}/Doxyfile"
echo ${sedPattern1}
# ^ means first of line
# \s means space or tab
# \s* means match one or more space or tabs
# . means match character
# .* means match one or more characters
# $ means match end of line
# () surround the part to keep (not literal match)
# \1 means match the first capture group from ()
#
# So replace:
# PROJECT_NAME       = programVersion        -> specific version like 5.1.4.8
sed "s/^\(PROJECT_NUMBER\s*=\s\)ProgramVersion.*$/\1${programVersion}/" ${doxyfileTemplate} > ${doxyfile}
# Don't need this since OUTPUT_DIRECTORY default is the starting folder, which is OK
#sed "s/^\(OUTPUT_DIRECTORY\s*=\s\)OutputDirectory*$/\1${doxygenWorkFolderRelative}/" > ${doxyfile}
if [ "$?" -ne 0 ]; then
	echo ""
	echo "Error updating Doxyfile-template to Doxyfile"
	echo "Exiting."
	exit 1
fi

# Change into the working folder to run Doxygen
echo "Changing to final Doxygen work folder to run Doxygen with updated Doxyfile:"
echo "  ${doxygenWorkFolder}"
cd ${doxygenWorkFolder}

# Finally, run the Doxygen UI
# - it should automatically pickup on the Doxyfile
# - TODO smalers 2018-10-23 evaluate whether there should be able to run in batch mode
echo ""
echo "The Doxygen software will now be started."
$echo2 "${okColor}Use the File / Open to open the file:${endColor}"
$echo2 "${okColor}  ${doxyfile}${endColor}"
$echo2 "${okColor}Then use the Run tab / Run doxygen button to run the software.${endColor}"
$echo2 "${okColor}Then use the Show HTML Output button to show documentation in a browser.${endColor}"
$echo2 "${okColor}Closing Doxygen will exit this script.${endColor}"
doxywizard
if [ "$?" -ne 0 ]; then
	echo ""
	echo "Error running Doxygen. Is software installed correctly?"
	exit 1
fi

exit 0
