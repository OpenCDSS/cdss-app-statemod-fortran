#!/bin/bash
#
# Count the number of warnings for specific warning types.
# This is helpful when trying to produce "clean" code.
# Run in the folder with source code.

# Name of log file.
compileLogFile="statemod-compile.log"

if [ ! -f "${compileLogFile}" ]; then
  echo "No log file is available to analyze:  ${compileLogFile}"
  echo "Run 'make help' for instructions on how to save compile output to log file."
  echo ""
  echo "  make -k statemod 2>&1 | tee statemod-compile.log"
  echo ""
  exit 1
fi

sumCountWarnings=0
echo "=====   ============================================================"
echo "Count : Warning text (searches are case-insensitive)"
echo "=====   ============================================================"

count=$(grep -i 'CHARACTER expression will be truncated ' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : CHARACTER expression will be truncated"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'is used uninitialized' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : is used uninitialized"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'Label ' ${compileLogFile} | grep 'defined but not used' | wc -l)
echo "$(printf %5d ${count}) : Label xxx at (y) defined but not used"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'Line truncated' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : Line truncated"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'may be used uninitialized' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : may be used uninitialized"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'Possible change of value' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : Possible change of value"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'Unused dummy argument' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : Unused dummy argument"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

count=$(grep -i 'Unused variable' ${compileLogFile} | wc -l)
echo "$(printf %5d ${count}) : Unused variable"
sumCountWarnings=$((${count} + ${sumCountWarnings}))

# Search for general string, should match the sum of all strings.
count=$(grep -i 'Warning' ${compileLogFile} | grep -v -i 'format' | wc -l)
echo ""
echo "$(printf %5d ${count}) : Warning"

# Print the count of unaccounted for warnings.
countUnaccountedForWarnings=$((${count} - ${sumCountWarnings}))
echo ""
echo "Unaccounted for warnings : ${countUnaccountedForWarnings}"

exit 0
