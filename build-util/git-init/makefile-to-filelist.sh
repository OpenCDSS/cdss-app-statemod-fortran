#!/bin/sh
#
# Output the list of filenames in the makefile_windows file, simply to get a list of files being compiled
# - the list can be compared to that from the AUTOMAKE.RSP.
# - run in the folder where makefile_windows exists
# - create files containing .for and .o file lists

# First create the file list with .o extensions

processMakefile () {

makefile=$1

cat ${makefile} | awk '
# Useful functions
# Trim whitespace from left side of string
function ltrim(s) { sub(/^[ \t\r\n]+/, "", s); return s }
# Trim whitespace from right side of string
function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
# Trim whitespace from both sides of string
function trim(s)  { return rtrim(ltrim(s)); }

BEGIN {
	foundStateMod = "false" # Whether statemod: is found, which is the start of .o file list
}
{
	# Strip the line
	aLine = trim($0)
	firstToken = trim($1)
	if ( firstToken == "statemod:" ) {
		foundStateMod = "true"
		#printf("Found statemod:\n" )
	}
	else if ( index(aLine,"$(FC)") >= 1 ) {
		# Done processing data
		exit 0
	}
	else if ( foundStateMod == "true" ) {
		# In the StaetMod file list so print the file
		printf("%s\n",firstToken)
		#printf("%s %s\n",aLine, foundStateMod)
	}
}
'| sort -u > ${makefile}-o-filelist.txt

# Next create the file list with .for extensions

while read f;
do
	# Replace .o with .for in filename
	ffor="${f/.o/.for}"
	echo ${ffor}
done < ${makefile}-o-filelist.txt > ${makefile}-for-filelist.txt
}

# Process the Windows and Linux makefiles using the above function

processMakefile "makefile_windows"
processMakefile "makefile_linux"
