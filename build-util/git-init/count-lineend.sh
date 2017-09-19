#!/bin/sh
#
# Count the number of line ends in each file in a folder
# - This is important because formatting issues may occur if developers use different line ending
# - Editors and git (with .gitattributes) should handle but is helpful to know for sure

# List all files to be examined

outfileTxt=endline-count-for.txt
outfileCsv=endline-count-for.csv

echo "file countLines countCRLF endType" > ${outfileTxt}
ls -1 | (while read f;
do
	# For each file count the CR-LF and LF only

	# The following counts the number of tabs in each line
	# - output is space-separated numbers
	#`awk '{print gsub(/\t/,"")}' ${f}`
	#
	# The following prints the number of lines that have a tab
	# - cat the file because otherwise wc echoes the filename
	# The following does not seem to work - does not count last line if has no \n
	lineCountWC=`cat ${f} | wc -l`
	lineCount=`grep -c "" ${f}`
	# The following does not seem to work - always returns 0
	#linesWithCR=`grep "" ${f} | wc -l`
	linesWithCR=`grep -U $'\r' ${f} | wc -l`
	if [ ${linesWithCR} = "0" ]
		then
		if [ ${lineCountWC} != ${lineCount} ]
			then
			endType="noeol"
		else
			endType="LF"
		fi
	elif [ ${lineCount} = ${linesWithCR} ]
		then
		endType="CRLF"
	else
		endType="mixed"
	fi
	echo ${f} ${lineCount} ${linesWithCR} ${endType}

done) >> ${outfileTxt}
cat ${outfileTxt} | tr " " "," > ${outfileCsv}

echo "See output files ${outfileTxt} and ${outfileCsv}"
