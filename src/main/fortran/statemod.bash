#!/bin/bash
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
# This is a generic script to run StateMod so that the user does not need to type the long executable name:
# - this script can be distributed with a StateMod dataset in the 'StateMod' folder,
#   with '.bash' extension removed
# - long executable name is similar to:  statemod-14.0.0-gfortran-win-64bit.exe
# - the longer name is required to uniquely identify the executable version and avoid confusion,
#   especially during transition from older versions and to be consistent with StateMod naming 

# Supporting functions, alphabetized.

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
#     statemod-17.0.0-gfortran-win-64bit.exe
# - in most cases, a dataset folder will only contain a small number of executables,
#   or perhaps only one
getStatemodExe() {
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
  # - TODO smalers 2021-09-02 this does simple sorting, need to implement more robust version sort
  #exeCount=$('ls' -1 ${statemodPattern} | sort --reverse | grep -v '32' | grep -v 'o3' | grep -v 'check' | wc -l 2> /dev/null)
  # The following lists the most recent version first.
  exeCount=$(listStatemodExes ${scriptFolder} | wc -l)
  echoStderr "Found ${exeCount} StateMod exectables.  Running the most recent version, based on sorting by version."
  if [ "${exeCount}" -eq 0 ]; then
    # Could not find a candidate executable.
    echoStderr "Could not find any production StateMod executables matching ${statemodPattern}"
    return 1
  else
    # List again and echo the first one, which should be the largest version:
    # - quote the command to disable alias
    #'ls' -1 ${statemodPattern} 2> /dev/null | sort --reverse | grep -v '32' | grep -v 'o3' | grep -v 'check' | head -1
    listStatemodExes ${scriptFolder} | head -1
    return 0
  fi
}

# List StateMod executables sorted by version number:
# - only the executable name is provided (since folder is known from input)
# - the newest is listed first
# - sorting modifies the version in the installer path to force the correct order
# - first parameter is the 'StateMod' dataset folder
# - versions older than 14 will probably be listed with invalid filename do to removal of zero-padded parts
# - version 14 is the first version to use non-zero-padded version and is required for this script,
#   ignore all other versions
listStatemodExes() {
  local statemodFolder
  statemodFolder=${1}

  if [ ! -d "${statemodFolder}" ]; then
    # Probably should not have been called.  Output nothing.
    echoStderr "StateMod folder does not exist: ${statemodFolder}"
    return 0
  fi

  # Using find makes sure that a full path is output and controls the depth,
  # in case someone has created sub-folders to back up executables, etc.
  find ${statemodFolder} -mindepth 1 -maxdepth 1 -type f -name '*statemod*exe' | awk -v debug=${debug} '
    BEGIN {
      if ( debug == "true" ) {
        print "[DEBUG] debug=" debug >> "/dev/stderr"
      }
    }
    {
    # Pad the software version in the listing with zeros to allow
    # a correct numerical sort on the version parts.  Original listing is similar to:
    #  /C/Users/user/cdss-dev/StateMod/git-repos/cdss-app-statemod-fortran-test/test/datasets/cm2015_StateMod/exes/statemod-17.0.0-gfortran-win-64bit/StateMod/statemod-17.0.0-gfortran-win-64bit.exe
    if ( debug == "true" ) {
      # Only do this when troubleshooting because it prints to the same stream as the output list.
      print "[DEBUG] First awk input: " $0 >> "/dev/stderr"
    }
    # To allow some flexibility, assume the filename follows the pattern:
    #   xxxxx1.2.3xxxxxx
    # - version starts on the first digit and ends with a dash
    # - this allows 'beta', 'dev', etc. to be in the version
    # Split the path.
    nPathParts=split($0,pathParts,"/")
    # Exe is the last part.
    exeFile=pathParts[nPathParts]
    if ( debug == "true" ) {
      print "[DEBUG] exeFile=" exeFile >> "/dev/stderr"
    }
    # Split all characters in the exe filename into an array.
    nExeFileChars=split(exeFile,exeFileChars,"")
    if ( debug == "true" ) {
      print "[DEBUG] nExeFileChars=" nExeFileChars >> "/dev/stderr"
    }
    # index start and positions, inclusive
    indexVersionStart=0
    indexVersionEnd=0
    for ( i = 1; i <= nExeFileChars; i++ ) {
      c = exeFileChars[i]
      # Uncomment for troubleshooting:
      # - this is very verbose but needed in some cases
      if ( debug == "true" ) {
        print "[DEBUG] processing character=" c >> "/dev/stderr"
      }
      # Check if character is a digit:
      # - if so save it
      # - also indicate that in the version
      if ( indexVersionStart > 1 ) {
        # Processing characters in the version:
        # - check for the end of the version
        if ( c == "-" ) {
          # Assume detected the end of the version.
          indexVersionEnd = (i - 1)
          if ( debug == "true" ) {
            print "[DEBUG] Detected end of version at " indexVersionEnd >> "/dev/stderr"
          }
          break
        }
        else {
          # Any character allowed.
          continue
        }
      }
      else if ( c ~ /^[0-9]+$/ ) {
        # Assume the start of the version.
        indexVersionStart = i
        if ( debug == "true" ) {
          print "[DEBUG] Detected start of version at " indexVersionStart >> "/dev/stderr"
        }
      }
    }
    version=substr(exeFile,indexVersionStart,(indexVersionEnd - indexVersionStart + 1))
    if ( debug == "true" ) {
      print "[DEBUG] version=" version >> "/dev/stderr"
    }
    # Split the version parts and then regenerate as padded for sorting.
    nVersionParts=split(version,versionParts,".")
    # There can be 3 or 4 parts:
    # - pad each part to 3 digits
    if ( nVersionParts == 2 ) {
      # Old style like 13.10
      # - fake a third and fourth part.
      versionPadded=sprintf("%04d.%04d.zzz.zzz", versionParts[1], versionParts[2] )
    }
    else if ( nVersionParts == 3 ) {
      # New style with no fourth part:
      # - fake a fourth part to force sort before "dev1", etc.
      versionPadded=sprintf("%04d.%04d.%04d.zzz", versionParts[1], versionParts[2], versionParts[3] )
    }
    else {
      # New style with fourth part that can be any lenth and type.
      versionPadded=sprintf("%04d.%04d.%04d.%s", versionParts[1], versionParts[2], versionParts[3], versionParts[4] )
    }
    if ( debug == "true" ) {
      print "[DEBUG] versionPadded=" versionPadded >> "/dev/stderr"
    }
    # Output is only the filename without leading path.
    startString=substr(exeFile,1,(indexVersionStart-1))
    endString=substr(exeFile,(indexVersionEnd+1))
    printf("%s%s%s\n", startString, versionPadded, endString)
  }' | sort -r | awk -v debug=${debug} '{
    # Process the version back to the original non-padded numbers.
    # Input is well-behaved versions like:
    # xxxxx0013.0010.zzz.zzz-xxxxx...               # Old
    # xxxxx0014.0000.0000.zzz.zzz-xxxxx...          # New with no 4th part
    # xxxxx0014.0002.0000.dev-xxxxx...              # New with 4th part
    if ( debug == "true" ) {
      print "[DEBUG] Second awk input: " $0 >> "/dev/stderr"
    }
    exeFile=$0
    # Split all characters in the exe filename into an array.
    nExeFileChars=split(exeFile,exeFileChars,"")
    # index start and positions, inclusive
    indexVersionStart=0
    indexVersionEnd=0
    for ( i = 1; i <= nExeFileChars; i++ ) {
      c = exeFileChars[i]
      # Check if character is a digit:
      # - if so save it
      # - also indicate that in the version
      if ( indexVersionStart > 1 ) {
        # Processing characters in the version:
        # - check for the end of the version
        if ( c == "-" ) {
          # Assume detected the end of the version.
          indexVersionEnd = (i - 1)
          break
        }
        else {
          # Any character allowed.
          continue
        }
      }
      else if ( c ~ /^[0-9]+$/ ) {
        # Assume the start of the version.
        indexVersionStart = i
      }
    }
    # Version can be extracted based on the above.
    version=substr(exeFile,indexVersionStart,(indexVersionEnd - indexVersionStart + 1))
    if ( debug == "true" ) {
      print "[DEBUG] version=" version >> "/dev/stderr"
    }
    # Split the version into parts:
    # - version, for example: 0001.0013.0000
    nVersionParts=split(version,versionParts,".")
    # There should always be 4 parts.
    if ( versionParts[3] == "zzz" && versionParts[4] == "zzz" ) {
      # Remove the dummy third and fourth part.
      versionUnpadded=sprintf("%d.%d", versionParts[1], versionParts[2] )
    }
    else if ( versionParts[4] == "zzz" ) {
      # Remove the dummy fourth part.
      versionUnpadded=sprintf("%d.%d.%d", versionParts[1], versionParts[2], versionParts[3] )
    }
    else {
      # Keep the dummy fourth part.
      versionUnpadded=sprintf("%d.%d.%d.%s", versionParts[1], versionParts[2], versionParts[3], versionParts[4] )
    }
    if ( debug == "true" ) {
      print "[DEBUG] versionUnpadded=" versionUnpadded >> "/dev/stderr"
    }
    startString=substr($0,1,(indexVersionStart-1))
    endString=substr($0,(indexVersionEnd+1))
    printf("%s%s%s\n", startString, versionUnpadded, endString)
  }'
  return 0
}

# Get the folder where this script is located since it may have been run from any folder
scriptFolder=$(cd $(dirname "$0") && pwd)

# This script version:
# - used when inspecting the code
# - do not implement command line parsing because don't want to confuse with StateMod options
version="1.0.0 (2021-09-02)"

# Debug must be set by modifying this script because no --debug command line parameter.
debug="false"
#debug="true"

# Change to the script folder just in case not already there:
# - the StateMod executable should be in the 'StateMod' folder of the dataset
echoStderr "Running in folder:"
echoStderr "  ${scriptFolder}"
cd ${scriptFolder}

statemodExe=$(getStatemodExe)
if [ $? -eq 0 ]; then
  # Success determining StateMod executable:
  # - run the program with the original command line options
  if [ -n "${statemodExe}" ]; then
    # Have an executable name.
    echoStderr ""
    echoStderr "Running StateMod executable:  '${statemodExe}' $@"
    echoStderr "If necessary, kill the process with: CTRL-c"
    echoStderr "If necessary, run a different StateMod executable using its file name."
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
