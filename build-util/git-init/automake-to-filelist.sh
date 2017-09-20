#!/bin/sh
#
# Convert the filenames in the automake.rsp file to lowercase and change extensions
# - run in the folder where AUTOMAKE.RSP file exists
# - output file automake-o-filelist.txt contains the list of .o files
# - output file automake-for-filelist.txt contains the list of .for files

# First create the list of .o files

(while read f;
do
	fLower=`echo ${f} | tr '[A-Z]' '[a-z]'`
	fLower="${fLower/.obj/.o}"
	echo ${fLower}
done < AUTOMAKE.RSP) | sort > automake-o-filelist.txt

# Next create the list of .for files

while read f;
do
        # Replace .o with .for in filename
        ffor="${f/.o/.for}"
        echo ${ffor}
done < automake-o-filelist.txt > automake-for-filelist.txt

echo "See output files automake-for-filelist.txt and automake-o-filelist.txt"
