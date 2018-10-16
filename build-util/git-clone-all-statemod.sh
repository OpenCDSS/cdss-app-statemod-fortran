#!/bin/sh
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
# git-clone-all-statemod - clone all StateMod repositories for new development environment setup
# - this script calls the general git utilities script

# Get the location where this script is located since it may have been run from any folder
# -see: https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
# -see: https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e
# -the following should work if no symbolic links are involved, but no link should be used here so use the simple solution
scriptDir=`dirname "$0"`

# StateMod product home is relative to the user's files in a standard CDSS development files location
# - $HOME/${productHome}
productHome="cdss-dev/StateMod"

# StateMod GitHub repo URL root
githubRootUrl="https://github.com/OpenWaterFoundation"

# Main StateMod repository
mainRepo="cdss-app-statemod-fortran"

# Run the general script
${scriptDir}/git-util/git-clone-all.sh -m "${mainRepo}" -p "${productHome}" -u "${githubRootUrl}" $@
