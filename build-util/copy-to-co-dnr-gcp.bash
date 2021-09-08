#!/bin/bash
#
# Create and copy the StateCU executable zip file installer to the CO DNR GCP website:
# - replace all the files on the web for the specific version with local files

# Supporting functions, alphabetized.

# Determine the operating system that is running the script:
# - mainly care whether Cygwin
checkOperatingSystem() {
  if [ ! -z "${operatingSystem}" ]; then
    # Have already checked operating system so return.
    return
  fi
  operatingSystem="unknown"
  os=`uname | tr [a-z] [A-Z]`
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
  echoStderr "[INFO] operatingSystem=${operatingSystem} (used to check for Cygwin and filemode compatibility)"
}

# Create a zip file containing the StateMod files:
# - the ${statemodVersion} must have been set
createZip() {
  local sevenZip

  if [ -z "${statemodVersion}" ]; then
    echoStderr "[ERROR] StateCU version is not set.  Cannot create zip file."
    return 1
  fi

  # Create a temporary folder to hold the files.
  echoStderr "[INFO] Building zip file in:"
  echoStderr "[INFO]   ${buildFolder}"
  if [ -d "${buildFolder}" ]; then
    # Remove it so that the contents will be current.
    rm -rf ${buildFolder}
    if [ -d "${buildFolder}" ]; then
      # Unsuccessful removing.
      echoStderr "[ERROR] Could not remove old build folder:"
      echoStderr "[ERROR]   ${buildFolder}"
      return 1
    fi
  fi
  # The folder was removed.  Recreate it.
  mkdir -p ${buildFolder2}
  # Copy files into the folder:
  # - rename the bash script to remove the extension 
  cp ${srcMainFolder}/statemod-${statemodVersion}-gfortran-win-64bit.exe ${buildFolder2}
  cp ${srcMainFolder}/statemod-${statemodVersion}-gfortran-win-64bit-check.exe ${buildFolder2}
  cp ${srcMainFolder}/statemod.cmd ${buildFolder2}
  cp ${srcMainFolder}/statemod.bash ${buildFolder2}/statecu
  # Zip the file using 7zip.
  sevenZip="/C/Program Files/7-Zip/7z.exe"
  if [ ! -f "${sevenZip}" ]; then
    # Major problem.
    echoStderr "[ERROR] Can't find 7-zip software to create zip file:"
    echoStderr "[ERROR]   ${sevenZip}"
    return 1
  fi
  cd ${buildFolder}
  # Zip the distribution folder that contains software files.
  "${sevenZip}" a -tzip ${zipFile} $(basename ${buildFolder2})
  if [ $? -ne 0 ]; then
    echoStderr "[ERROR] Error running 7-zip."
    return 1
  else
    echoStderr "[INFO] Created zip file:"
    echoStderr "[INFO]   ${zipFile}"
  fi

  return 0
}

# Echo a string to standard error (stderr).
# This is done so that output printed to stdout is not mixed with stderr.
echoStderr() {
  echo "$@" >&2
}

# Get the version modifier:
# - for example, from "12.00.00dev", "12.00.00 dev", 12.00.00beta", or "12.00.00 beta"
# - the first function argument is the full version, possibly including a space
# - the modifier is echoed, so capture by assigning in the calling code
getVersionModifier() {
  local fullVersion
  fullVersion="$1"
  # grep will print each found character on a separate line so concatenate output.
  modifier=$(echo ${fullVersion} | grep -o -E '[[:alpha:]]' | tr -d '\n' | tr -d ' ')
  echo ${modifier}
}

# Echo to stderr:
# - if necessary, quote the string to be printed
# - this function is called to print various message types
echoStderr() {
  echo "$@" 1>&2
}

# Parse the command parameters:
# - use the getopt command line program so long options can be handled
parseCommandLine() {
  local optstring optstringLong
  local exitCode
  local GETOPT_OUT

  # Single character options.
  optstring="dhlv"
  # Long options.
  optstringLong="copy-to-latest,debug,dryrun,help,version"
  # Parse the options using getopt command.
  GETOPT_OUT=$(getopt --options ${optstring} --longoptions ${optstringLong} -- "$@")
  exitCode=$?
  if [ ${exitCode} -ne 0 ]; then
    # Error parsing the parameters such as unrecognized parameter.
    echoStderr ""
    printUsage
    exit 1
  fi
  # The following constructs the command by concatenating arguments.
  eval set -- "${GETOPT_OUT}"
  # Loop over the options.
  while true; do
    #echo "Command line option is ${opt}"
    case "${1}" in
      --debug) # --debug  Indicate to output debug messages.
        echoStderr "--debug detected - will print debug messages."
        debug="true"
        shift 1
        ;;
      -d|--dryrun) # -d or --dryrun  Indicate to do a dryrun but not actually upload.
        echoStderr "--dryrun detected - will not change files on GCP"
        dryrun="-n"
        shift 1
        ;;
      -h|--help) # -h or --help  Print the program usage.
        printUsage
        exit 0
        ;;
      --l,--copy-to-latest) # --l or --copy-to-latest  Also copy to "latest".
        echoStderr "--copy-to-latest detected."
        copyToLatest="yes"
        shift 1
        ;;
      -v|--version) # -v or --version  Print the program version.
        printVersion
        exit 0
        ;;
      --) # No more arguments.
        shift
        break
        ;;
      *) # Unknown option.
        echoStderr ""
        echoStderr "Invalid option: ${1}" >&2
        printUsage
        exit 1
        ;;
    esac
  done
}

# Print the usage.
printUsage() {
  echoStderr ""
  echoStderr "Usage: ${scriptName} [options]"
  echoStderr ""
  echoStderr "Copy the StateCU installer (zip file) to the versioned website folder:"
  echoStderr "  ${gsFolderVersion}"
  echoStderr "Optionally, copy the StateCU installer (zip file) to 'latest' website folder if -l specified:"
  echoStderr "  ${gsFolderLatest}"
  echoStderr ""
  echoStderr "--debug                Turn on debug for troubleshooting."
  echoStderr "-d, --dryrun           Dry run (print actions but don't execute upload)."
  echoStderr "-h, --help             Print usage."
  echoStderr "-l, --copy-to-latest   Copy to 'latest' folder in addition to auto-detected version folder."
  echoStderr "-v, --version          Print the program version."
  echoStderr ""
}

# Print the version.
printVersion() {
  echoStderr "${version}"
}

# Set the location of the 'gsutil' script:
# - Google provides a linux shell script and 'gsutil.cmd' is available for Windows
# - MinGW does not seem to inherit the full Windows PATH so find the script in typical location
# - set the global ${gsutilCommand} variable
setGsutilCommand() {
  local gsutilPath

  echoStderr "Check if 'gsutil' is in the PATH."
  gsutilCommand=$(which gsutil)
  if [ $? -eq 0 ]; then
    # Found it.
    return 0
  else
    # Look in the standard location.
    gsutilPath="/c/Users/${USER}/AppData/Local/Google/Cloud SDK/google-cloud-sdk/bin/gsutil"
    echoStderr "Check whether 'gsutil' is in the normal location:"
    echoStderr "  ${gsutilPath}"
    if [ ! -f "${gsutilPath}" ]; then
      # Error is printed in calling code.
      echoStderr "[ERROR] Unable to find 'gsutil' - check that it is installed."
      gsutilCommand=""
      return 1
    else
      gsutilCommand="${gsutilPath}"
      echoStderr "[INFO] Will use gsutil:"
      echoStderr "[INFO]   ${gsutilCommand}"
    fi
  fi
}

# Sync the files to GCP.
syncFiles() {
  # Copy the local files up to Google Cloud:
  # - the -m option causes operations to run in parallel, which can be much faster
  # - the -d option means delete extra files in destination
  # - the -r option means recursive to sync the whole folder tree
  # TODO smalers 2021-09-02 actually, only need "latest" for documentation.
  #if [ ${copyToLatest} = "yes" ]; then
  #  if [ -f "${exeFileGfortran}" ]; then
  #    # TODO smalers 2019-04-29 need some logic to remove the previous version:
  #    # - can't do full remove because may have Linux version
  #    gsutil.cmd cp ${dryrun} ${exeFileGfortran} ${gsFileLatestGfortran}
  #  else
  #    echo "File does not exist for 'latest' upload:  ${exeFileGfortran}"
  #  fi
  #fi
  # Upload to the versioned copy.
  echoStderr "Confirm copy:"
  echoStderr "  from: ${zipFile}"
  echoStderr "    to: ${gsFileVersionZip}"
  read -p "Continue with upload (Y/n/q)? " answer
  if [ -z "${answer}" -o "${answer}" = "y" -o "$answer" = "Y" ]; then
    "${gsutilCommand}" cp ${dryrun} ${zipFile} ${gsFileVersionZip}
    return $?
  elif [ "${answer}" = "q" -o "$answer" = "Q" ]; then
    exit 0
  else
    # Can still rebuild index.
    return 0
  fi
}

# Update the GCP index that lists files.
updateIndex() {
  local answer
  echo ""
  read -p "Do you want to update the GCP index file [Y/n]? " answer
  if [ -z "${answer}" -o "${answer}" = "y" -o "$answer" = "Y" ]; then
    ${scriptFolder}/create-gcp-statemod-index.bash
  fi
  return 0
}

# Entry point for the script.

# Check the operating system.
checkOperatingSystem

# Get the location where this script is located since it may have been run from any folder.
scriptFolder=$(cd $(dirname "$0") && pwd)
scriptName=$(basename $0)
version="1.0.0 (2021-09-05)"

repoFolder=$(dirname "${scriptFolder}")
srcFolder="${repoFolder}/src"
srcMainFolder="${srcFolder}/main/fortran"
statemSrcFile="${srcMainFolder}/statem.for"
if [ -f "${statemSrcFile}" ]; then
  statemodVersion=$(cat ${statemSrcFile} | grep 'ver =' | grep -v 'xx' | cut -d '=' -f 2 | sed "s/'//g" | tr -d ' ')
  statemodModifierVersion=$(getVersionModifier "${statemodVersion}")
else
  echoStderr "[ERROR] Cannot determine StateMod version because file not found:  ${statemSrcFile}"
  exit 1
fi
if [ -z "${statemodVersion}" ]; then
  echoStderr "[ERROR] Cannot determine StateMod version by scanning:  ${statemSrcFile}"
  exit 1
fi

# TODO smalers 2021-07-20 have moved to 64-bit executable so don't even deal with 32-bit anymore.
bits=64
exeFileGfortran="${srcMainFolder}/statemod-${statemodVersion}-gfortran-win-${bits}bit.exe"
echoStderr "scriptFolder=${scriptFolder}"
echoStderr "repoFolder=${repoFolder}"
echoStderr "srcFolder=${srcFolder}"
echoStderr "srcMainFolder=${srcMainFolder}"
echoStderr "statemSrcFile=${statemSrcFile}"
echoStderr "statemodVersion=${statemodVersion}"
echoStderr "exeFileGfortran=${exeFileGfortran}"

dryrun=""
# TODO smalers 2021-09-05 latest is only for documentation.
#gsFolderLatest="gs://opencdss.state.co.us/statemod/latest/software"
# Use standard file name.
gsFolderVersion="gs://opencdss.state.co.us/statemod/${statemodVersion}/software"
if [ "${operatingSystem}" = "mingw" ]; then
  # MinGW is used for Git Bash and also StateMod MinGW development environment.
  #gsFileLatestGfortran="${gsFolderLatest}/statemod-${statemodVersion}-gfortran-win-${bits}bit.exe"
  gsFileVersionZip="${gsFolderVersion}/statemod-cdss-${statemodVersion}.zip"
else
  echoStderr "[ERROR]"
  echoStderr "[ERROR] Don't know how to handle operating system:  ${operatingSystem}"
  exit 1
fi

# Whether to copy to latest in addition to the specific version:
# - default to no because the script can be run on any version, and can't assume latest
copyToLatest="no"

# Parse the command line.
parseCommandLine $@

if [ ! -z "${statemodVersionModifier}" -a "${copyToLatest}" = "yes" ]; then
  # The version contains "dev" or "beta" so don't allow to be used for "latest".
  echo "StateMod version ${statemodVersion} contains modifier - not copying to latest."
  copyToLatest="no"
fi

# Set the location of gsutil command:
# - MinGW uses the linux script rather than Windows gsutil.cmd
setGsutilCommand
if [ $? -ne 0 -o -z "${gsutilCommand}" ]; then
  echoStderr "[ERROR] Unable to find 'gsutil'.  Exiting."
  exit 1
fi

# Create zip file:
# - this is needed because unlike TSTool, the build process does not currently create the installer
createZip
exitStatus=$?

# Sync the files to the cloud.
if [ ${exitStatus} -eq 0 ]; then
  syncFiles
  exitStatus=$?
fi

# Also update the index.
if [ ${exitStatus} -eq 0 ]; then
  updateIndex
  exitStatus=$?
fi

# Exist with the status from the most recent call above.
exit ${exitStatus}
