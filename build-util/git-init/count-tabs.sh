#!/bin/sh
#
# Count the number of tabs in each Fortran file
# - This is important because formatting issues may occur if developers use different tab width
# - To search for ^Z using grep on the command line,
#   use the following ^v^z to enter the control character: grep "^Z" file

# List all files to be examined
ls -1 *.for *.FOR *.For *.inc *.INC | (while read f;
do
	# For each file count the lines that have tabs

	# The following counts the number of tabs in each line
	# - output is space-separated numbers
	#`awk '{print gsub(/\t/,"")}' ${f}`
	#
	# The following prints the number of lines that have a tab
	# - cat the file because otherwise wc echoes the filename
	lineCount=`cat ${f} | wc -l`
	linesWithTabCount=`grep "	" ${f} | wc -l`
	echo ${f} ${linesWithTabCount} ${lineCount}

done) > tabcount-for.txt
cat tabcount-for.txt | tr " " "," > tabcount-for.csv
