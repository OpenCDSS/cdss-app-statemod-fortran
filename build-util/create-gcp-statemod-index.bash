#!/bin/bash
#
# Create the following files on GCP bucket:
#
#   opencdss.state.co.us/statemod/index.html
#   opencdss.state.co.us/statemod/index.csv
#
# The 'index.html' file is used as the default web page.
# The 'index.csv' file can be used by StateMod GUI and other tools to check for updates.
#
# MSys2 MinGW does not use the full Windows Path so the 'gsutil' program must be located.
# MSys2 MinGW does seem to be shipped with Python3, so it can be run from 'gsutil'.

# Supporting functions, alphabetized.

# Determine the operating system that is running the script:
# - sets the variable operatingSystem to 'cygwin', 'linux', or 'mingw' (Git Bash)
checkOperatingSystem() {
  if [ ! -z "${operatingSystem}" ]; then
    # Have already checked operating system so return
    return
  fi
  operatingSystem="unknown"
  os=$(uname | tr [a-z] [A-Z])
  case "${os}" in
    CYGWIN*)
      operatingSystem="cygwin"
      operatingSystemShort="cyg"
      ;;
    LINUX*)
      operatingSystem="linux"
      operatingSystemShort="lin"
      ;;
    MINGW*)
      operatingSystem="mingw"
      operatingSystemShort="min"
      ;;
  esac
  echoStderr ""
  echoStderr "Detected operatingSystem=${operatingSystem} operatingSystemShort=${operatingSystemShort}"
  echoStderr ""
}

# Echo a string to standard error (stderr).
# This is done so that output printed to stdout is not mixed with stderr.
echoStderr() {
  echo "$@" >&2
}

# Check whether a file exists on GCP storage:
# - function argument should be Google storage URL gs:opencdss... etc.
gcpUtilFileExists() {
  local fileToCheck
  fileToCheck=$1
  # The following will return 0 if the file exists, 1 if not
  #gsutil.cmd -q stat $fileToCheck
  ${gsutilCommand} -q stat ${fileToCheck}
  return $?
}

# Get the user's login to use for local temporary files:
# - Git Bash apparently does not set ${USER} environment variable
# - Set USER as script variable only if environment variable is not already set
# - See: https://unix.stackexchange.com/questions/76354/who-sets-user-and-username-environment-variables
getUserLogin() {
  if [ -z "${USER}" ]; then
    if [ ! -z "${LOGNAME}" ]; then
      USER=${LOGNAME}
    fi
  fi
  if [ -z "${USER}" ]; then
    USER=$(logname)
  fi
  # Else - not critical since used for temporary files.
}

# Parse the command parameters:
# - use the getopt command line program so long options can be handled
parseCommandLine() {
  local optstring optstringLong
  local exitCode
  local GETOPT_OUT

  # Single character options.
  optstring="dhv"
  # Long options.
  optstringLong="debug,dryrun,help,version"
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
        echoStderr "--dryrun detected - will not change files on GCP."
        dryrun="-n"
        shift 1
        ;;
      -h|--help) # -h or --help  Print the program usage.
        printUsage
        exit 0
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
  echoStderr "Usage: ${scriptName}"
  echoStderr ""
  echoStderr "Create the product GCP index file:"
  echoStderr "  ${gcpIndexHtmlUrl}"
  echoStderr ""
  echoStderr "--debug           Print debug messages, for troubleshooting."
  echoStderr "-h, --help        Print usage."
  echoStderr "-v, --version     Print script version."
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
    # Found it
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

# Upload the index.html file for CDSS download page:
# - this is basic at the moment but can be improved in the future 
uploadIndexHtmlFile() {
  local indexHtmlTmpFile gcpIndexHtmlUrl
  local indexCsvTmpFile gcpIndexCsvlUrl
  # List available software installer files:
  # - $gcpFolderUrl ends with /statemod
  # - the initial output will look like the following, with size, timestamp, resource URL:
  #
  # gs://opencdss.state.co.us/statemod/17.0.0/software/:
  #         11  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/17.0.0/software/
  #   94612246  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/17.0.0/software/statemod-cdss-17.0.0.zip
  #
  # gs://opencdss.state.co.us/statemod/latest/software/:
  #   94612246  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/17.0.0/software/statemod-cdss-17.0.0.zip
  # TOTAL: 2 objects, 94612246 bytes (90.27 MiB)
  #
  #   after filtering, the output looks like the following:
  #
  # 94612246  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/17.0.0/software/statemod-cdss-17.0.0.zip
  # 94612246  2019-04-27T10:01:47Z  gs://opencdss.state.co.us/statemod/latest/software/statemod-cdss-17.0.0.zip

  # First do a listing of the software folder.
  tmpGcpSoftwareCatalogPath="/tmp/${USER}-statemod-software-catalog-ls.txt"
  echoStderr "tmpGcpSoftwareCatalogPath=${tmpGcpSoftwareCatalogPath}"
  # Match zip and tar.gz files to include Windows and Linux.
  #gsutil.cmd ls -l "${gcpFolderUrl}/*/software" | grep -E -v '^gs*' | grep gs | grep -E 'zip|tar.gz' > ${tmpGcpSoftwareCatalogPath}
  "${gsutilCommand}" ls -l "${gcpFolderUrl}/*/software" | grep -E -v '^gs*' | grep gs | grep -E 'zip|tar.gz' > ${tmpGcpSoftwareCatalogPath}
  if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echoStderr ""
    echoStderr "[Error] Error listing StateMod download files to create catalog."
    exit 1
  fi

  # Listing of the doc-user folders.
  tmpGcpDocUserCatalogPath="/tmp/${USER}-statemod-doc-user-catalog-ls.txt"
  echoStderr "tmpGcpDocUserCatalogPath=${tmpGcpDocUserCatalogPath}"
  # Match index.html paths.
  #gsutil.cmd ls -l "${gcpFolderUrl}/*/doc-user" | grep -E -v '^gs*' | grep gs | grep index.html > ${tmpGcpDocUserCatalogPath}
  "${gsutilCommand}" ls -l "${gcpFolderUrl}/*/doc-user" | grep -E -v '^gs*' | grep gs | grep index.html > ${tmpGcpDocUserCatalogPath}
  if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echoStderr ""
    echoStderr "[Error] Error listing StateMod user documentation files to create catalog."
    exit 1
  fi

  # Listing of the doc-dev folders.
  tmpGcpDocDevCatalogPath="/tmp/${USER}-statemod-doc-dev-catalog-ls.txt"
  echoStderr "tmpGcpDocDevCatalogPath=${tmpGcpDocDevCatalogPath}"
  # Match index.html paths.
  #gsutil.cmd ls -l "${gcpFolderUrl}/*/doc-dev" | grep -E -v '^gs*' | grep gs | grep index.html > ${tmpGcpDocDevCatalogPath}
  "${gsutilCommand}" ls -l "${gcpFolderUrl}/*/doc-dev" | grep -E -v '^gs*' | grep gs | grep index.html > ${tmpGcpDocDevCatalogPath}
  if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echoStderr ""
    echoStderr "[Error] Error listing StateMod developer documentation files to create catalog."
    exit 1
  fi

  # Create an index.html file for upload.
  indexHtmlTmpFile="/tmp/${USER}-statemod-index.html"
  gcpIndexHtmlUrl="${gcpFolderUrl}/index.html"

  # Also create index as CSV to facilitate listing available versions.
  indexCsvTmpFile="/tmp/${USER}-statemod-index.csv"
  gcpIndexCsvUrl="${gcpFolderUrl}/index.csv"
  echo '<!DOCTYPE html>
<head>
<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
<meta http-equiv="Pragma" content="no-cache" />
<meta http-equiv="Expires" content="0" />
<meta charset="utf-8"/>
<link id="cdss-favicon" rel="shortcut icon" href="https://opencdss.state.co.us/opencdss/images/opencdss-favicon.ico" type="image.ico">
<style>
   body {
     font-family: "Trebuchet MS", Helvetica, sans-serif !important;
   }
   table {
     border-collapse: collapse;
   }
   th {
     border-right: solid 1px;
     border-left: solid 1px;
     border-bottom: solid 1px;
     padding-left: 5px;
     padding-right: 5px;
   }
   td {
     border-right: solid 1px;
     border-left: solid 1px;
     padding-left: 5px;
     padding-right: 5px;
   }
   #installersize {
     border-right: solid 1px;
     border-left: solid 1px;
     padding-left: 5px;
     padding-right: 5px;
     text-align: right;
   }
   tr {
     border: none;
   }
</style>
<title>OpenCDSS StateMod Downloads</title>
</head>
<body>
<h1>StateMod Software Downloads</h1>
<p>
<a href="https://opencdss.state.co.us/opencdss/statemod/">See also the OpenCDSS StateMod page</a>,
which provides additional information about StateMod.
</p>
<p>
<a href="https://www.colorado.gov/pacific/cdss/statemod">See also the CDSS StateMod page</a>,
which provides access to StateMod releases used in State of Colorado projects.
</p>
<p>
The StateMod software is available for Windows and Linux.
See the <a href="https://opencdss.state.co.us/statemod/latest/doc-user/">latest StateMod documentation</a>
for installation information (or follow a documentation link below for specific version documentation).
<!-- The StateMod software is typically packaged with the StateMod Graphical User Interface (StateMod GUI)
and can be included in the StateMod folder of datasets to ensure that a dataset is run with compatible version of StateMod.-->
StateMod software can be included in the StateMod folder of datasets to ensure that a dataset is run with compatible version of StateMod.
</p>
<p>
<ul>
<li>The StateMod version has three parts (e.g., 14.0.0).</li>
<li>The Windows installer zip files include the following files:
  <ul>
  <li>StateMod optimized executable, for general use (e.g.,
      <code>statemod-17.0.0-gfortran-win-64bit.exe</code>) - this file should be included in "StateMod" folder of datasets</li>
  <li>StateMod executable with runtime checks enabled, for troubleshooting
      (e.g., <code>statemod-17.0.0-gfortran-win-64bit-check.exe</code>) - this executable can be run during troubleshooting</li>
  <li>StateMod runner for Windows, which provides a generic name and runs the newest executable
      (<code>statemod.cmd</code>) - this file should be included in "StateMod" folder of datasets</li>
  <li>StateMod runner for Linux, which provides a generic name and runs the newest executable
      (<code>statemod</code>) - this file should be included in "StateMod" folder of datasets to
      support use on Linux-like environments such as Cygwin, MinGW, and Git Bash</li>
  </ul></li>
<li><b>If clicking on a file download link does not download the file,
right-click on the link and use "Save link as..." (or similar).</b></li>
</ul>

<hr>
<h2>Windows Download</h2>
<p>
Install by downloading the zip file and then use Windows File Explorer or other software to install.
For example, copy to a model dataset "StateMod" folder.
Then run "statemod" to run a simulation.
</p>' > ${indexHtmlTmpFile}

  # Generate a table of available versions for Windows.

  uploadIndexHtmlFile_Table win Windows

  # TODO smalers 2019-04-29 need to enable downloads for other operating systems.

echo '<hr>
<h2>Linux Download</h2>
<p>
Linux versions of StateMod are not currently provided by the State of Colorado.
Contact the <a href="https://openwaterfoundation.org">Open Water Foundation</a> if interested.
</p>' >> ${indexHtmlTmpFile}

  echo '</body>' >> ${indexHtmlTmpFile}
  echo '</html>' >> ${indexHtmlTmpFile}

  echoStderr ""
  echoStderr "Uploading the index file:"
  echoStderr "  from: ${indexHtmlTmpFile}"
  echoStderr "    to: ${gcpIndexHtmlUrl}"
  echoStderr ""
  read -p "Continue with upload (Y/n)? " answer
  if [ -z "${answer}" -o "${answer}" = "Y" -o "${answer}" = "y" ]; then
    # Will continue.
    :
  else
    # Exit the script.
    exit 0
  fi

  # If here, continue with the copy.
  # set -x
  #gsutil.cmd cp ${indexHtmlTmpFile} ${gcpIndexHtmlUrl}
  "${gsutilCommand}" cp ${indexHtmlTmpFile} ${gcpIndexHtmlUrl}
  # { set +x; } 2> /dev/null
  if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echoStderr ""
    echoStderr "[Error] Error uploading index.html file."
    exit 1
  fi

  # Similarly create the index.csv file.

  # First create the index:
  # - use some string replacements to make sure that dev versions are listed after non-dev release
  #   with the same number is listed first
  echo '"Version"' > ${indexCsvTmpFile}
  cat ${tmpGcpSoftwareCatalogPath} | awk '{print $3}' | cut -d '/' -f 5 | sed -E 's/([0-9]$)/\1-zzz/g' | sed 's/0dev/0-dev/g' | sort -r | sed 's/-zzz//g' >> ${indexCsvTmpFile}

  # Then upload to the website.
  #gsutil.cmd cp ${indexCsvTmpFile} ${gcpIndexCsvUrl}
  "${gsutilCommand}" cp ${indexCsvTmpFile} ${gcpIndexCsvUrl}
  if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echoStderr ""
    echoStderr "[Error] Error uploading index.csv file."
    exit 1
  fi
}

# Create a table of downloads for an operating system to be used in the index.html file:
# - first argument is operating system short name to match installers:  "win", "lin", or "cyg"
# - second argument is operating system long name to match installers:  "Windows", "Linux", or "Cygwin"
uploadIndexHtmlFile_Table() {
  local downloadOs dowloadPattern
  local indexHtmlTmpCatalogFile
  # Operating system is passed in as the required first argument.
  downloadOs=$1
  downloadOsLong=$2
  # The following allows sorting the list in reverse order.
  indexHtmlTmpCatalogFile="/tmp/${USER}-statemod-catalog-${downloadOs}.html"
  if [ "${downloadOs}" = "win" ]; then
    downloadPattern="zip"
  elif [ "${downloadOs}" = "lin" ]; then
    downloadPattern="tar.gz"
  fi
  if [ "${debug}" = "true" ]; then
    echoStderr "[DEBUG] downloadPattern=${downloadPattern}"
  fi
  echo '<table>' >> ${indexHtmlTmpFile}
  # List the available download files.
  # Listing local files does not show all available files on GCP but may be useful for testing.
  catalogSource="gcp"  # "gcp" or "local"
  if [ "${catalogSource}" = "gcp" ]; then
    # Use GCP list from catalog file for the index.html file download file list, with format like
    # the following (no space at beginning of the line):
    #
    # 12464143  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/17.0.0/software/statemod-cdss-17.0.0.zip
    #
    # Use awk below to print the line with single space between tokens.
    # Replace normal version to have -zzz at end and "dev" version to be "-dev" so that sort is correct,
    #   then change back to previous strings for output.
    # The use space as the delimiter and sort on the 3rd token.
    #
    echo '<tr><th>Download File</th><th>Product</th><th>Version</th><th>File Timestamp</th><th>Size (bytes)</th><th>Operating System</th><th>User Doc</th><th>Dev Doc</th></tr>' >> ${indexHtmlTmpFile}
    cat "${tmpGcpSoftwareCatalogPath}" | grep "${downloadPattern}" | awk '{ printf "%s %s %s\n", $1, $2, $3 }' | sed -E 's|([0-9][0-9]/)|\1-zzz|g' | sed 's|/-zzz|-zzz|g' | sed 's|dev|-dev|g' | sort -r -k3,3 | sed 's|-zzz||g' | sed 's|-dev|dev|g' | awk -v debug=${debug} -v tmpGcpDocUserCatalogPath=${tmpGcpDocUserCatalogPath} -v tmpGcpDocDevCatalogPath=${tmpGcpDocDevCatalogPath} '
      # By the time input is provided, the installer files are sorted with newest first.
      # Debug messages are saved as comments in the HTML.
      BEGIN {
        if ( debug == "true" ) {
          printf("<!-- [DEBUG] tmpGcpDocUserCatalogPath=%s -->\n", tmpGcpDocUserCatalogPath)
          printf("<!-- [DEBUG] tmpGcpDocDevCatalogPath=%s -->\n", tmpGcpDocDevCatalogPath)
        }
      }
      {
        # Download file is the full line.
        downloadFileSize = $1
        downloadFileDateTime = $2
        downloadFilePath = $3
        # Split the download file path into parts to get the download file without path:
        # - index is 1+
        nparts=split(downloadFilePath,downloadFilePathParts,"/")
        downloadFile = downloadFilePathParts[nparts]
        downloadFileUrl=downloadFilePath
        gsub("gs:","https:",downloadFileUrl)
        # Split the download file into parts to get other information:
        # - file name is like:  statemod-cdss-17.0.0.zip
        # - index is 1+
        split(downloadFile,downloadFileParts,"-")
        #downloadFileProduct=downloadFileParts[1]
        downloadFileProduct="StateMod"
        # Version needs to remove '.zip', based on check below.
        downloadFileVersion=downloadFileParts[3]
        #downloadFileCompiler=downloadFileParts[3]
        if ( substr(downloadFile,(length(downloadFile) - 3)) == ".zip") {
          if ( debug == "true" ) {
            printf("<!-- [DEBUG] Detected Windows installer. -->\n" );
          }
          downloadFileOs="Windows"
          downloadFileVersion=substr(downloadFileVersion,1,(length(downloadFileVersion) - 4))
        }
        else if ( substr(downloadFile,(length(downloadFile) - 6)) == ".tar.gz" ) {
          if ( debug == "true" ) {
            printf("<!-- [DEBUG] Detected Linux installer. -->\n" );
          }
          downloadFileOs="Linux"
          downloadFileVersion=substr(downloadFileVersion,1,(length(downloadFileVersion) - 7))
        }
        else {
          if ( debug == "true" ) {
            printf("<!-- [DEBUG] Unknown operating system for installer. -->\n" );
          }
        }
        if ( debug == "true" ) {
          printf("<!-- [DEBUG] Download file=%s -->\n", downloadFile );
          printf("<!-- [DEBUG] Download file version=%s -->\n", downloadFileVersion );
        }

        # --- User documentation.
        cmd=sprintf("cat %s | grep 'statemod/%s/doc-user/index.html' | wc -l", tmpGcpDocUserCatalogPath, downloadFileVersion)
        cmd | getline docCount
        if ( debug == "true" ) {
          printf("<!-- [DEBUG] User documentation command=%s -->\n", cmd );
          printf("<!-- [DEBUG] User documentation count=%d -->\n", docCount );
        }
        if ( docCount > 0 ) {
          docUserUrl=sprintf("https://opencdss.state.co.us/statemod/%s/doc-user",downloadFileVersion)
          docUserHtml=sprintf("<a href=\"%s\">View</a>",docUserUrl)
        }
        else {
          docUserHtml=""
        }

        # --- Developer  documentation.
        cmd=sprintf("cat %s | grep 'statemod/%s/doc-dev/index.html' | wc -l", tmpGcpDocDevCatalogPath, downloadFileVersion)
        cmd | getline docCount
        if ( debug == "true" ) {
          printf("<!-- [DEBUG] Dev documentation command=%s -->\n", cmd );
          printf("<!-- [DEBUG] Dev documentation count=%d\n -->", docCount );
        }
        if ( docCount > 0 ) {
          docDevUrl=sprintf("https://opencdss.state.co.us/statemod/%s/doc-dev",downloadFileVersion)
          docDevHtml=sprintf("<a href=\"%s\">View</a>",docDevUrl)
        }
        else {
          docDevHtml=""
        }

        #if ( downloadFileOs == "cyg" ) {
        #  downloadFileOs = "Cygwin"
        #}
        #else if ( downloadFileOs == "lin" ) {
        #  downloadFileOs = "Linux"
        #}
        #else if ( downloadFileOs == "win" ) {
        #  downloadFileOs = "Windows"
        #}
        printf "<tr><td><a href=\"%s\"><code>%s</code></a></td><td>%s</td><td>%s</td><td>%s</td><td id=\"installersize\">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n", downloadFileUrl, downloadFile, downloadFileProduct, downloadFileVersion, downloadFileDateTime, downloadFileSize, downloadFileOs, docUserHtml, docDevHtml
      }' >> ${indexHtmlTmpFile}
  fi
  echo '</table>' >> ${indexHtmlTmpFile}
}

# Entry point for the script.

# Get the location where this script is located since it may have been run from any folder.
scriptFolder=$(cd $(dirname "$0") && pwd)
scriptName=$(basename $0)
repoFolder=$(dirname "${scriptFolder}")
srcFolder="${repoFolder}/src"

# Version, mainly used to help understand changes over time when comparing files.
version="1.1.0 (2021-09-05)"

echo "scriptFolder=${scriptFolder}"
echo "repoFolder=${repoFolder}"
echo "srcFolder=${srcFolder}"

# Whether or not debug messages are printed.
debug="false"

# Whether or not dry run is done.
dryrun=""

# Root location where files are to be uploaded
gcpFolderUrl="gs://opencdss.state.co.us/statemod"

# Parse command line parameters.
parseCommandLine $@

# Determine the user login, used for temporary file location.
getUserLogin

# Set the location of gsutil command:
# - MinGW uses the linux script rather than Windows gsutil.cmd
setGsutilCommand
if [ $? -ne 0 -o -z "${gsutilCommand}" ]; then
  echoStderr "[ERROR] Unable to find 'gsutil'.  Exiting."
  exit 1
fi

# Upload the created index file to GCP bucket.
uploadIndexHtmlFile

exit $?
