#!/bin/bash
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
# This is a generic script to run StateMod so that the user does not need to type the long executable name:
# - this script can be distributed with a StateMod dataset in the 'StateMod' folder,
#   with '.bash' extension removed
# - long executable name is similar to:  statemod-16.00.48-gfortran-win-64bit.exe
# - the longer name is required to uniquely identify the executable version and avoid confusion,
#   especially during transition from Lahey to gfortran

# Supporting functions, alphabetized...

# Determine the operating system that is running the script:
# - mainly care whether Cygwin or MINGW (Git Bash)
checkOperatingSystem() {
  if [ ! -z "${operatingSystem}" ]; then
    # Have already checked operating system so return.
    return
  fi
  operatingSystem="unknown"
  os=$(uname | tr [a-z] [A-Z])
  case "${os}" in
    CYGWIN*)
      operatingSystem="cygwin"
      ;;
    LINUX*)
      operatingSystem="linux"
      ;;
    MINGW*)
      operatingSystem="mingw"
      ;;
  esac
}

# Echo to stderr:
# - if necessary, quote the string to be printed
# - this function is called to print various message types
echoStderr() {
  echo "$@" 1>&2
}

# Determine the StateMod executable.
# - echo the matched StateMod executable to stdout so it can be assigned in calling code
# - this seems to work OK with variations on the executable name:
#     statemod-16.00.47-gfortran-win-32bit.exe
#     statemod-16.00.47-gfortran-win-64bit.exe
#     statemod-16.00.48-gfortran-win-64bit-check.exe
#     statemod-16.00.48-gfortran-win-64bit-o3.exe
#     statemod-16.00.48-gfortran-win-64bit.exe
#     statemod-17.0.0-gfortran-win-64bit.exe
#     statemod-17.1.0-gfortran-win-64bit.exe
# - in most cases, a dataset folder will only contain a small number of executables
getStateModExe() {
  local exeCount statemodPattern

  exeCount=0
  if [ "${operatingSystem}" = "windows" ]; then
    # Windows:
    # - file extension can be used
    statemodPattern="statemod-*.exe"
  else
    # Any linux variant:
    # - more open-ended, may need to check other criteria
    statemodPattern="statemod-*"
  fi
  # First get a list of matching files:
  # - quote the command to disable alias
  exeCount=$('ls' -1 ${statemodPattern} | sort --reverse | grep -v '32' | grep -v 'o3' | grep -v 'check' | wc -l 2> /dev/null)
  if [ "${exeCount}" -eq 0 ]; then
    # Could not find a candidate executable.
    echoStderr "Could not find any StateMod executables matching ${statemodPattern}"
    return 1
  else
    # List again and save the first one, which should be the largest version:
    # - quote the command to disable alias
    'ls' -1 ${statemodPattern} | sort --reverse | grep -v '32' | grep -v 'o3' | grep -v 'check' | head -1
#2> /dev/null
    return 0
  fi
}

# Get the folder where this script is located since it may have been run from any folder
scriptFolder=$(cd $(dirname "$0") && pwd)

# This script version:
# - used when inspecting the code
# - do not implement command line parsing because don't want to confuse with StateMod options
version="1.0.0 (2021-07-20)"

# Change to the script folder just in case not already there:
# - the StateMod executable should be in the 'StateMod' folder of the dataset
cd ${scriptFolder}

statemodExe=$(getStateModExe)
if [ $? -eq 0 ]; then
  # Success determining StateMod executable:
  # - run the program with the original command line options
  if [ -n "${statemodExe}" ]; then
    # Have an executable name.
    echoStderr ""
    echoStderr "Running StateMod executable:  '${statemodExe}' $@"
    echoStderr "If necessary, kill the process with: CTRL-c"
    echoStderr "If necessary, run a StateMod executable using the longer file name."
    echoStderr ""
    ./${statemodExe} $@
    exit $?
  else
    # No executable name.  Should have resulted in return status of 1.
    printError
    exit 1
  fi
else
  # Error determining StateMod executable.
  printError
  exit 1
fi
