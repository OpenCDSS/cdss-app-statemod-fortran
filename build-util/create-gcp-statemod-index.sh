#!/bin/sh
#
# Create the opencdss.state.co.us/statemod/index.html file

# Supporting functions, alphabetized

# Determine the operating system that is running the script
# - sets the variable operatingSystem to cygwin, linux, or mingw (Git Bash)
checkOperatingSystem()
{
	if [ ! -z "${operatingSystem}" ]; then
		# Have already checked operating system so return
		return
	fi
	operatingSystem="unknown"
	os=`uname | tr [a-z] [A-Z]`
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
	echo ""
	echo "Detected operatingSystem=$operatingSystem operatingSystemShort=$operatingSystemShort"
	echo ""
}

# Check whether a file exists on GCP storage
# - function argument should be Google storage URL gs:opencdss... etc.
gcpUtilFileExists() {
	local fileToCheck
	fileToCheck=$1
	# The following will return 0 if the file exists, 1 if not
	gsutil.cmd -q stat $fileToCheck
	return $?
}

# Parse the command parameters
parseCommandLine() {
	while getopts :dhl opt; do
		#echo "Command line option is ${opt}"
		case $opt in
			d) # Indicate that this should be copied to the latest release and version
				dryrun="-n"
				;;
			h) # Usage
				printUsage
				exit 0
				;;
			l) # Indicate that this should be copied to the latest release and version
				copyToLatest="yes"
				;;
			\?)
				echo "Invalid option:  -$OPTARG" >&2
				exit 1
				;;
			:)
				echo "Option -$OPTARG requires an argument" >&2
				exit 1
				;;
		esac
	done
}

# Print the usage
printUsage() {
	echo ""
	echo "Usage:  $0"
	echo ""
	echo "Create the opencdss.state.co.us/statemod/index.html file."
	echo ""
}

# Upload the index.html file for the download static website
# - this is basic at the moment but can be improved in the future such as
#   software.openwaterfoundation.org page, but for only one product, with list of variants and versions
uploadIndexHtmlFile() {
	local indexHtmlTmpFile gcpIndexHtmlUrl
	# List available software installer files
	# - $gcpFolderUrl ends with /statemod
	# - the initial output will look like the following, with size, timestamp, resource URL:
	#
	# gs://opencdss.state.co.us/statemod/15.00.14/software/:
	#   12464143  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/15.00.14/software/statemod-15.00.14-gfortran-32bit.exe
	#
	# gs://opencdss.state.co.us/statemod/latest/software/:
	#   12464143  2019-04-27T10:01:47Z  gs://opencdss.state.co.us/statemod/latest/software/statemod-15.00.14-gfortran-32bit.exe
	# TOTAL: 2 objects, 24928286 bytes (23.77 MiB)
	#
	#   after filtering, the output looks like the following:
	#
	# 12464143  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/15.00.14/software/statemod-15.00.14-gfortran-32bit.exe
	# 12464143  2019-04-27T10:01:47Z  gs://opencdss.state.co.us/statemod/latest/software/statemod-15.00.14-gfortran-32bit.exe
	# TODO smalers 2019-04-29 need to use Bash PIPESTATUS for error code
	tmpGcpCatalogPath="/tmp/$USER-statemod-catalog-ls.txt"
	gsutil.cmd ls -l "${gcpFolderUrl}/*/software" | grep -E -v '^gs*' | grep gs > $tmpGcpCatalogPath; errorCode=$?
	if [ $errorCode -ne 0 ]; then
		echo ""
		echo "[Error] Error listing StateMod download files to create catalog."
		exit 1
	fi
	# Create an index.html file for upload
	indexHtmlTmpFile="/tmp/$USER-statemod-index.html"
	gcpIndexHtmlUrl="${gcpFolderUrl}/index.html"
	echo '<!DOCTYPE html>' > $indexHtmlTmpFile
	echo '<head>' >> $indexHtmlTmpFile
	echo '<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />' >> $indexHtmlTmpFile
	echo '<meta http-equiv="Pragma" content="no-cache" />' >> $indexHtmlTmpFile
	echo '<meta http-equiv="Expires" content="0" />' >> $indexHtmlTmpFile
	echo '<meta charset="utf-8"/>' >> $indexHtmlTmpFile
	echo '<style>' >> $indexHtmlTmpFile
	echo '   body { font-family: "Trebuchet MS", Helvetica, sans-serif !important; }' >> $indexHtmlTmpFile
	echo '   table { border-collapse: collapse; }' >> $indexHtmlTmpFile
	echo '   tr { border: none; }' >> $indexHtmlTmpFile
	echo '   th {' >> $indexHtmlTmpFile
	echo '     border-right: solid 1px;' >> $indexHtmlTmpFile
	echo '     border-left: solid 1px;' >> $indexHtmlTmpFile
	echo '     border-bottom: solid 1px;' >> $indexHtmlTmpFile
	echo '   }' >> $indexHtmlTmpFile
	echo '   td {' >> $indexHtmlTmpFile
	echo '     border-right: solid 1px;' >> $indexHtmlTmpFile
	echo '     border-left: solid 1px;' >> $indexHtmlTmpFile
	echo '   }' >> $indexHtmlTmpFile
	echo '</style>' >> $indexHtmlTmpFile
	echo '<title>StateMod Downloads</title>' >> $indexHtmlTmpFile
	echo '</head>' >> $indexHtmlTmpFile
	echo '<body>' >> $indexHtmlTmpFile
	echo '<h1>StateMod Software Downloads</h1>' >> $indexHtmlTmpFile
	echo '<p>' >> $indexHtmlTmpFile
	echo 'The StateMod software is available for Linux and Windows 10.' >> $indexHtmlTmpFile
	echo 'See the <a href="http://opencdss.state.co.us/statemod/latest/doc-user/">StateMod documentation</a> for installation information (or follow link below for the documentation for the specific version).' >> $indexHtmlTmpFile
	echo '</p>' >> $indexHtmlTmpFile
	echo '<p>' >> $indexHtmlTmpFile
	echo '<ul>' >> $indexHtmlTmpFile
	echo '<li>Multiple versions of StateMod can be installed on a computer, and should be used consistent with the version needed for a dataset.' >> $indexHtmlTmpFile
	echo '<li>Download files that include <code>dev</code> in the version are development versions that can be installed to test the latest features and bug fixes that are under development.</li>' >> $indexHtmlTmpFile
	echo '<li><b>If clicking on a file download link does not download the file, right-click on the link and use "Save link as..." (or similar).</b></li>' >> $indexHtmlTmpFile
	echo '</ul>' >> $indexHtmlTmpFile

	echo '<hr>' >> $indexHtmlTmpFile
	echo '<h2>Windows Download</h2>' >> $indexHtmlTmpFile
	echo '<p>' >> $indexHtmlTmpFile
	echo 'Install StateMod by downloading the executable file and saving in the dataset <code>bin</code> folder.' >> $indexHtmlTmpFile
	echo 'Then run StateMod from the Windows command prompt or use a helper script in the dataset.' >> $indexHtmlTmpFile
	echo '</p>' >> $indexHtmlTmpFile
	# Generate a table of available versions for Windows
	uploadIndexHtmlFile_Table win Windows

	# TODO smalers 2019-04-29 need to enable downloads for other operating systems
	echo '<hr>' >> $indexHtmlTmpFile
	echo '<h2>Linux Download</h2>' >> $indexHtmlTmpFile
	echo '<p>' >> $indexHtmlTmpFile
	echo 'Install StateMod by downloading the executable file and saving in the dataset <code>bin</code> folder.' >> $indexHtmlTmpFile
	echo 'Then run StateMod from the Linux command line or use a helper script in the dataset.' >> $indexHtmlTmpFile
	echo '</p>' >> $indexHtmlTmpFile
	# Generate a table of available versions for Linux
	uploadIndexHtmlFile_Table lin Linux

	#echo '<hr>' >> $indexHtmlTmpFile
	#echo '<h2>Cygwin Download</h2>' >> $indexHtmlTmpFile
	#echo '<p>' >> $indexHtmlTmpFile
	#echo 'Install the GeoProcessor on Cygwin by downloading the <a href="download-gp.sh">download-gp.sh script</a> and running it in a shell window.' >> $indexHtmlTmpFile
	#echo 'You will be prompted for options for where to install the software.' >> $indexHtmlTmpFile
	#echo 'Once installed, run the GeoProcessor using scripts in the <code>scripts</code> folder under the install folder.' >> $indexHtmlTmpFile
	#echo '<b>Do not download directly using files below (the list is provided as information).</b>' >> $indexHtmlTmpFile
	#echo '</p>' >> $indexHtmlTmpFile
	## Generate a table of available versions for Cygwin
	#uploadIndexHtmlFile_Table cyg Cygwin

	echo '</body>' >> $indexHtmlTmpFile
	echo '</html>' >> $indexHtmlTmpFile
	# set -x
	gsutil.cmd cp $indexHtmlTmpFile $gcpIndexHtmlUrl ; errorCode=$?
	# { set +x; } 2> /dev/null
	if [ $errorCode -ne 0 ]; then
		echo ""
		echo "[Error] Error uploading index.html file."
		exit 1
	fi
}

# Create a table of downloads for an operating system to be used in the index.html file.
uploadIndexHtmlFile_Table() {
	# Operating system is passed in as the required first argument
	downloadOs=$1
	downloadOsLong=$2
	echo '<table>' >> $indexHtmlTmpFile
	# List the available download files
	# Listing local files does not show all available files on GCP but may be useful for testing
	catalogSource="gcp"  # "gcp" or "local"
	if [ "$catalogSource" = "gcp" ]; then
		# Use GCP list from catalog file for the index.html file download file list, with format like
		# the following (no space at beginning of the line):
		#
		# 12464143  2019-04-27T10:01:42Z  gs://opencdss.state.co.us/statemod/15.00.14/software/statemod-15.00.14-gfortran-32bit.exe
		#
		echo '<tr><th>Download File</th><th>Product</th><th>Version</th><th>File Timestamp</th><th>Size (KB)</th><th>Operating System</th><th>User Doc</th><th>Dev Doc</th><th>API Doc</th></tr>' >> $indexHtmlTmpFile
		#cat "${tmpGcpCatalogPath}" | awk '
		## TODO smalers 2019-04-27 need to figure out how to check for documentation for each executable
		##		# Determine if matching documentation URL is valid
		##		docUserUrl0="gs://opencdss.state.co.us/statemod/${downloadFileVersion}/doc-user/index.html"
		##		if ! gcpUtilFileExists "$docUserUrl0"; then
		##			# Documentation is available so show link
		##			docUserUrl="<a href="http://opencdss.state.co.us/statemod/$downloadFileVersion/doc-user/">Doc</a>"
		##		else
		##			# No documentation available
		##			docUserUrl="-"
		##		fi
		cat "${tmpGcpCatalogPath}" | grep "${downloadOs}-" | sort -r | awk '
			{
				# Download file is the full line
				downloadFileSize = $1
				downloadFileDateTime = $2
				downloadFilePath = $3
				# Split the download file path into parts to get the download file without path
				nparts=split(downloadFilePath,downloadFilePathParts,"/")
				downloadFile = downloadFilePathParts[nparts]
				downloadFileUrl=downloadFilePath
				gsub("gs:","http:",downloadFileUrl)
				# Split the download file into parts to get other information
				split(downloadFile,downloadFileParts,"-")
				downloadFileProduct=downloadFileParts[1]
				downloadFileVersion=downloadFileParts[2]
				downloadFileCompiler=downloadFileParts[3]
				downloadFileOS=downloadFileParts[4]
				downloadFileArch=downloadFileParts[5]
				docDevUrl=""
				docUserUrl=""
				docApiUrl=""
				if ( downloadFileOs == "cyg" ) {
					downloadFileOs = "Cygwin"
				}
				else if ( downloadFileOs == "lin" ) {
					downloadFileOs = "Linux"
				}
				else if ( downloadFileOs == "win" ) {
					downloadFileOs = "Windows"
				}
				printf "<tr><td><a href=\"%s\"><code>%s</code></a></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", downloadFileUrl, downloadFile, downloadFileProduct, downloadFileVersion, downloadFileDateTime, downloadFileSize, downloadFileOs, docUserUrl, docDevUrl, docApiUrl
			}' >> $indexHtmlTmpFile
	fi
	echo '</table>' >> $indexHtmlTmpFile
}

z() {
	# Copy the local files up to Google Cloud
	# - the -m option causes operations to run in parallel, which can be much faster
	# - the -d option means delete extra files in destination
	# - the -r option means recursive to sync the whole folder tree
	if [ ${copyToLatest} = "yes" ]; then
		gsutil.cmd cp ${dryrun} $exeFileGfortran32 ${gsFileLatestGfortran32}
	fi
	# For now always upload to the versioned copy
	gsutil.cmd cp ${dryrun} $exeFileGfortran32 ${gsFileVersionGfortran32}
}

# Entry point for the script

# Get the location where this script is located since it may have been run from any folder
scriptFolder=`cd $(dirname "$0") && pwd`
repoFolder=$(dirname "$scriptFolder")
srcFolder="$repoFolder/src"

echo "scriptFolder=$scriptFolder"
echo "repoFolder=$repoFolder"
echo "srcFolder=$srcFolder"

dryrun=""

# Root location where files are to be uploaded
gcpFolderUrl="gs://opencdss.state.co.us/statemod"

parseCommandLine

uploadIndexHtmlFile

exit $?
