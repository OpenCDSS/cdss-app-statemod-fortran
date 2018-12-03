#!/bin/sh
# Search StateMod code for enhancements, edits, etc. to get list of contributors.
# - this is used to get a list of copyright assignment agreements

outputFile="statemod-edits.log"
echo 'The following StateMod *.for and *.inc code contain "revise", "change", "modify", "enhance", "fix", or "remove"' > ${outputFile}
grep -i -e 'revise' -e 'change' -e 'modify' -e 'enhance' -e 'fix' -e 'remove' ../src/main/fortran/*.for ../src/main/fortran/*.inc >> ${outputFile}
