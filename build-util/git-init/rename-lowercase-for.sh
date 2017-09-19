#!/bin/sh
#
# Rename Fortran source files to lowercase.
# - Also remove ^Z (end of file) characters since not needed
# - To search for ^Z using grep on the command line,
#   use the following ^v^z to enter the control character: grep "^Z" file

ls -1 *.for *.FOR *.For | (while read f;
do
        fLower=`echo ${f} | tr '[A-Z]' '[a-z]'`
	if [ ${f} != ${fLower} ]
		then
		# Original and lowercase filename are different so rename
        	echo "mv ${f} ${fLower}"
		mv ${f} ${fLower}
	fi
	# Also remove ^Z if any instances exist (otherwise leave as is so as to preserve original modification time)
	# - corresponds to ascii code 26 = 1A = 032
	hasZ=`grep -P '\x1A' ${fLower} | wc --bytes`
	if [ ${hasZ} != "0" ]
		then
		# Found nonzero count of ^Z in the file.
		# Remove the ^Z to a temporary file and then rename back.
        	echo "Detected ctrl-z in ${fLower}...removing"
		sed 's///g' ${fLower} > ztemp
		mv ztemp ${fLower}
	fi
done)

# Remove the temporary file if it still exists

if [ -a "ztemp" ]
	then
	rm ztemp
fi
