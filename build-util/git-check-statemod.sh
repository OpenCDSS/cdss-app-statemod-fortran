#!/bin/sh
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
# git-check-statemod - check the StateMod repositories for status
# - this script calls the general git utilities script

# Get the location where this script is located since it may have been run from any folder
# -see: https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
# -see: https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e
# -the following should work if no symbolic links are involved, but no link should be used here so use the simple solution
scriptDir=`dirname "$0"`

# StateMod product home is relative to the user's files in a standard CDSS development files location
# - $HOME/${productHome}
productHome="cdss-dev/StateMod"

# Main StateMod repository
mainRepo="cdss-app-statemod-fortran"

# Run the general utility script
${scriptDir}/git-util/git-check.sh -m "${mainRepo}" -p "${productHome}" $@
