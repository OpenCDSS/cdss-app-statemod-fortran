#!/bin/sh
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
# git-check.sh
#
# Check the status of multiple repositories for this project and indicate whether pull
# or push or other action is needed.
# - see the main entry point at the bottom of the script for script configuration
# - currently must adhere to prescribed folder structure
# - useful when multiple repositories form a product
# - this script does not do anything to change repositories
# - warn if any repositories use Cygwin because mixing with Git for Windows can cause confusion in tools

# Determine the OS that is running the script
# - mainly care whether Cygwin
checkOperatingSystem()
{
	if [ ! -z "${operatingSystem}" ]
	then
		# Have already checked operating system so return
		return
	fi
	operatingSystem="unknown"
	case "$(uname)" in
		CYGWIN*) operatingSystem="cygwin";;
		MINGW*) operatingSystem="mingw";;
	esac
	echo "operatingSystem=$operatingSystem (used to check for Cygwin)"
	if [ "${operatingSystem}" = "cygwin" ]
	then
		echo "RECOMMEND not using Cygwin git commands for development for this product"
	fi
}

# Function to confirm that proper command-line Git client is being used.
# - mainly to confirm that Cygwin is not used when filemode=false
# - in order to see local config associated with a repo must cd into the repo folder
checkCommandLineGitCompatibility()
{
	# Make sure that the operating system has been determined
	# - will set operatingSystem to "cygwin" if cygwin
	checkOperatingSystem
	filemodeLine=`git config --list | grep filemode`
	#echo "${filemodeLine}"
	if [ ! -z "${filemodeLine}" ]
		then
		# filemode is usually printed by Git bash but may not be printed by Cygwin
		# - if repo was cloned using Git Bash:  core.filemode=false
		filemode=`echo $filemodeLine | cut --delimiter='=' --fields=2`
		#echo "repository filemode=$filemode"
		if [ "${filemode}" = "true" ]
		then
			# Count Cygwin repos so message can be printed at the end
			cygwinRepoCount=`expr ${cygwinRepoCount} + 1`
			#echo "cygwinRepoCount=${cygwinRepoCount}"
		fi
		if [ "${operatingSystem}" = "cygwin" ] && [ "${filemode}" = "false" ]
		then
			# Probably cloned using Git Bash or other Windows-centric Git client
			echo "DO NOT USE CygWin command line git with this repo (was NOT cloned with Cygwin, filemode=false)."
		elif [ "${operatingSystem}" = "cygwin" ] && [ "${filemode}" = "true" ]
		then
			# Probably cloned using Cygwin but for consistency recommend Windows-centric Git client
			echo "RECOMMEND re-cloning repo with Git Bash for consistency (was cloned with Cygwin, filemode=true)."
		elif [ "${operatingSystem}" = "mingw" ] && [ "${filemode}" = "true" ]
		then
			# Probably cloned using Cygwin but for consistency recommend Windows-centric Git client
			echo "USE CygWin command line git with this repo (was cloned with Cygwin, filemode=true)."
		elif [ "${operatingSystem}" = "mingw" ] && [ "${filemode}" = "false" ]
		then
			# Probably cloned using Git Bash or other Windows-centric Git client so OK
			echo "USE Git Bash or other Windows git client with this repo (filemode=false)."
		else
			echo "Unhandled operating system ${operatingSystem} - no git use recommendations provided."
		fi
	fi
}

# Check whether any working files are different from commits in the repo
# - have to include function before it is called
# - print a message if so
# - assumes that the current folder is a Git repository of interest
# - see:  https://stackoverflow.com/questions/3882838/whats-an-easy-way-to-detect-modified-files-in-a-git-workspace
checkWorkingFiles()
{
	# The following won't work if run in Cygwin shell for non-Cygwin repo clone, or the other way
	git diff-index --quiet HEAD
	# Therefore, use the following (although this could ignore newline cleanup that needs to be committed)
	# - however, even though --quiet is specified, the following outputs a bunch of messages like:
	#   warning:  CRLF will be replaced by LF in ...
	#   The file will have its original line endings in your working directory.
	#git diff-index --ignore-cr-at-eol --quiet HEAD
	exitCode=$?
	#echo "exitCode=$exitCode"
	if [ $exitCode -eq 1 ]
	then
		echo "Working files contain modified files that need to be committed, or staged files."
	fi
	# The above won't detect untracked files but the following will find those
	# - the following will return a value of 0 or greater
	untrackedFilesCount=`git ls-files -o --directory --exclude-standard | wc -l`
	if [ ${untrackedFilesCount} -ne "0" ]
	then
		echo "Working files contain ${untrackedFilesCount} untracked files that need to be committed."
	fi
	if [ $exitCode -eq 1 -o ${untrackedFilesCount} -ne "0" ]
	then
		localChangesRepoCount=`expr ${localChangesRepoCount} + 1`
	fi
}

# Function to check the status of local compared to remote repository
# - see:  https://stackoverflow.com/questions/3258243/check-if-pull-needed-in-git
checkRepoStatus()
{
	# Get the remote information
	git remote update
	# Start code from above StackOverflow article
	UPSTREAM=${1:-'@{u}'}
	LOCAL=$(git rev-parse @)
	REMOTE=$(git rev-parse "$UPSTREAM")
	BASE=$(git merge-base @ "$UPSTREAM")

	if [ "$LOCAL" = "$REMOTE" ]; then
		echo "------------------"
		echo "Up-to-date"
		checkWorkingFiles
		upToDateRepoCount=`expr ${upToDateRepoCount} + 1`
		echo "------------------"
	elif [ "$LOCAL" = "$BASE" ]; then
		echo "------------------"
		echo "Need to pull"
		checkWorkingFiles
		needToPullRepoCount=`expr ${needToPullRepoCount} + 1`
		echo "------------------"
	elif [ "$REMOTE" = "$BASE" ]; then
		echo "------------------"
		echo "Need to push"
		checkWorkingFiles
		needToPushRepoCount=`expr ${needToPushRepoCount} + 1`
		echo "------------------"
	else
		echo "------------------"
		echo "Diverged"
		checkWorkingFiles
		divergedRepoCount=`expr ${divergedRepoCount} + 1`
		echo "------------------"
	fi
	# End code from above StackOverflow article
}

# Entry point into main script
# - output some blank lines to make it easier to scroll back in window to see the start of output
echo ""
echo ""
echo ""

# Check the operating system
checkOperatingSystem

# Product development main folder
# - top-level folder for the product
home2=$HOME
if [ "${operatingSystem}" = "cygwin" ]
	then
	# Expect product files to be in Windows user files location (/cygdrive/...), not Cygwin user files (/home/...)
	home2="/cygdrive/C/Users/$USER"
fi
# StateMod product home is relative to the users files in a standard CDSS development files location
productHome="$home2/cdss-dev/StateMod"
# Main repository in a group of repositories for a product
# - this is where the product repository list file will live
mainRepo="${productHome}/git-repos/cdss-app-statemod-fortran"
# The following is a list of repositories including the main repository
# - one repo per line, no URL, just the repo name
# - repositories must have previously been cloned to local files
repoList="${mainRepo}/build-util/product-repo-list.txt"

# Check for local folder existence and exit if not as expected
# - ensures that other logic will work as expected in folder structure

if [ ! -d "${productHome}" ]
	then
	echo ""
	echo "Product home folder does not exist:  ${productHome}"
	echo "Exiting."
	echo ""
	exit 1
fi
if [ ! -d "${mainRepo}" ]
	then
	echo ""
	echo "Main repo folder does not exist:  ${mainRepo}"
	echo "Exiting."
	echo ""
	exit 1
fi
if [ ! -f "${repoList}" ]
	then
	echo ""
	echo "Product repo list file does not exist:  ${repoList}"
	echo "Exiting."
	echo ""
	exit 1
fi

# Count the number of Cygwin repositories so can remind developer at the end
cygwinRepoCount=0
upToDateRepoCount=0
needToPullRepoCount=0
needToPushRepoCount=0
divergedRepoCount=0
localChangesRepoCount=0

# Change folders to each repository and run the function to check that repository status
# against its upstream repository.
# - use syntax that does not use pipe so that internal variables are in same scope as main script
#   and can be processed after the loop
# - ignore comment lines starting with #
while IFS= read -r repoName
do
	if [ -z "${repoName}" ]
	then
		# Blank line
		continue
	fi
	firstChar=`expr substr "${repoName}" 1 1` 
	if [ "${firstChar}" = "#" ]
	then
		# Comment line
		continue
	fi
	# Check the status on the specific repository
	productRepoFolder="${productHome}/git-repos/${repoName}"
	echo "================================================================================"
	echo "Checking status of repo:  $repoName"
	if [ ! -d "${productRepoFolder}" ]
		then
		echo ""
		echo "Product repo folder does not exist:  ${productRepoFolder}"
		echo "Skipping."
		continue
	else
		# Change to repo folder (otherwise Git commands don't know what to do)
		cd ${productRepoFolder}
		checkRepoStatus
		# Check to make sure that proper Git command line tool is being used
		# - filemode=false indicates that Cygwin should not be used
		checkCommandLineGitCompatibility
	fi
#done 
done < ${repoList}

echo "================================================================================"
# Print a message to encourage not using Cygwin to clone repositories
if [ "${cygwinRepoCount}" -ne "0" ]
then
	echo "Number of Cygwin-cloned repos is ${cygwinRepoCount}.  See above for recommendations."
fi
# Print message to alert about attention needed on any repository
echo "Number of up-to-date repositories:                                        ${upToDateRepoCount}"
echo "Number of 'need to pull' repositories (remote commits available):         ${needToPullRepoCount}"
echo "Number of 'need to push' repositories (local commits saved):              ${needToPushRepoCount}"
echo "Number of diverged repositories (need to pull and push):                  ${divergedRepoCount}"
echo "Number of repositories with local changes (working and/or staged files):  ${localChangesRepoCount}"
echo "================================================================================"

# Done
exit 0
