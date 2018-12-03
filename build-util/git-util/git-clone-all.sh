#!/bin/sh
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is required
# The above line ensures that the script can be run on Cygwin/Linux even with Windows CRNL
#
#-----------------------------------------------------------------------------
# Git Utilities
# Copyright 2017-2018 Open Water Foundation.
# 
# License GPLv3+:  GNU GPL version 3 or later
# 
# There is ABSOLUTELY NO WARRANTY; for details see the
# "Disclaimer of Warranty" section of the GPLv3 license in the LICENSE file.
# This is free software: you are free to change and redistribute it
# under the conditions of the GPLv3 license in the LICENSE file.
#-----------------------------------------------------------------------------
#
# git-clone-all.sh
#
# Clone all repositories
# - this is used to set up a new development environment
# - it is assumed that the main repository is already cloned (since these files live there)

# Variables
dryRun=false # Default is to run operationally
#dryRun=true  # for testing

version="1.3.0 2018-11-27"

# Supporting functions

# Determine the operating system that is running the script
# - mainly care whether Cygwin
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
			;;
		LINUX*)
			operatingSystem="linux"
			;;
		MINGW*)
			operatingSystem="mingw"
			;;
	esac
	echo "operatingSystem=$operatingSystem (used to check for Cygwin and filemode compatibility)"
}

# Print the script usage
printUsage() {
	echo ""
	echo "Usage:  git-clone-all.sh -m productMainRepo -g gitReposFolder -u remoteRootUrl"
	echo ""
	echo "Example:"
	echo "    git-clone-all.sh -m owf-git-util -g $HOME/owf-dev/Util-Git/git-repos -u https://github.com/OpenCDSS"
	echo ""
	echo "-g specifies the folder containing 1+ Git repos for product."
	echo "-h prints the usage"
	echo "-m specifies the main repository name."
	echo "-u specifies the root URL where repositories will be found."
	echo "-v prints the version"
	echo ""
}

# Main script entry point

# Check the operating system to control logic
checkOperatingSystem

# Parse the command parameters
while getopts :g:hm:p:u:v opt; do
	#echo "Command line option is ${opt}"
	case $opt in
		g) # Folder containing Git repositories
			gitReposFolder=$OPTARG
			;;
		h) # usage
			printUsage
			exit 0
			;;
		m) # main repo
			mainRepo=$OPTARG
			;;
		p) # product home
			echo "-p is obsolete.  Use -g instead."
			;;
		u) # GitHub (or other) root URL
			remoteRootUrl=$OPTARG
			;;
		v) # version
			echo ""
			echo "git-clone-all version ${version}"
			echo ""
			echo "Git Utilities"
			echo "Copyright 2017-2018 Open Water Foundation."
			echo ""
			echo "License GPLv3+:  GNU GPL version 3 or later"
			echo ""
			echo "There is ABSOLUTELY NO WARRANTY; for details see the"
			echo "'Disclaimer of Warranty' section of the GPLv3 license in the LICENSE file."
			echo "This is free software: you are free to change and redistribute it"
			echo "under the conditions of the GPLv3 license in the LICENSE file."
			echo ""
			exit 0
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

if [ -z "${gitReposFolder}" ]; then
	echo ""
	echo "The Git repositories folder has not been specified with -g.  Exiting."
	echo ""
	exit 1
fi
if [ -z "${remoteRootUrl}" ]; then
	echo ""
	echo "The remote root URL has not been specified with -u.  Exiting."
	echo ""
	exit 1
fi

# Product development main folder
# - top-level folder for the product
home2=$HOME
if [ "${operatingSystem}" = "cygwin" ]; then
	# Expect product files to be in Windows user files location (/cygdrive/...), not Cygwin user files (/home/...)
	home2="/cygdrive/C/Users/$USER"
fi
# Git repsitories folder is relative to the user's files in a standard development location, for example:
# $HOME/                     User's files.
#    DevFiles/               Development files grouped by a system, product line, etc.
#      ProductHome/          Development files for a specific product.
#        git-repos/          Git repositories that comprise the product.
#          repo-name1/       Git repository folders (each containing .git, etc.)
#          repo-name2/
#          ...
#
# Main repository in a group of repositories for a product
# - this is where the product repository list file will live
mainRepoAbs="${gitReposFolder}/${mainRepo}"
# The following is a list of repositories including the main repository
# - one repo per line, no URL, just the repo name
# - repositories must have previously been cloned to local files
repoListFile="${mainRepoAbs}/build-util/product-repo-list.txt"

# Check for local folder existence and exit if not as expected
# - ensures that other logic will work as expected in folder structure

if [ ! -d "${mainRepoAbs}" ]; then
	echo ""
	echo "Main repo folder does not exist:  ${mainRepoAbs}"
	echo "Exiting."
	echo ""
	exit 1
fi
if [ ! -f "${repoListFile}" ]; then
	echo ""
	echo "Product repo list file does not exist:  ${repoListFile}"
	echo "Exiting."
	echo ""
	exit 1
fi

while [ "1" = "1" ]
do
	echo ""
	echo "Clone all repositories for the product to set up a new developer environment."
	echo "The following is from ${repoListFile}"
	echo ""
	echo "--------------------------------------------------------------------------------"
	cat ${repoListFile}
	echo "--------------------------------------------------------------------------------"
	echo ""
	echo "All repositories that don't already exist will be cloned to ${gitReposFolder}."
	echo "Repositories will be cloned using root URL ${remoteRootUrl}"
	echo "You may be prompted to enter credentials."
	read -p "Continue [y/n]?: " answer
	if [ "${answer}" = "y" ]
	then
		# Want to continue
		break
	else
		# Don't want to continue
		exit 0
	fi
done

# Change to the main git-repos folder
cd "${gitReposFolder}"
# Clone each repository in the product
while IFS= read -r repoName
do
	# Make sure there are no carriage returns in the string
	# - can happen because file may have Windows-like endings but Git Bash is Linux-like
	# - use sed because it is more likely to be installed than dos2unix
	repoName=`echo ${repoName} | sed 's/\r$//'`
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
	# Clone the repo
	repoFolder="${gitReposFolder}/${repoName}"
	repoUrl="${remoteRootUrl}/${repoName}"
	echo "================================================================================"
	echo "Cloning repository:  ${repoName}"
	echo "Repository folder:  ${repoFolder}"
	echo "Repository Url:  ${repoUrl}"
	if [ -d "${repoFolder}" ]
	then
		# The repository folder exists so don't cone
		echo "Repo folder already exists so skipping:  ${repoFolder}"
		continue
	else
		if [ ${dryRun} = "true" ]
		then
			echo "Dry run:  cloning repository with:  git clone \"${repoUrl}\""
		else
			git clone ${repoUrl}
		fi
	fi
done < ${repoListFile}
echo "================================================================================"

# List the repositories

echo ""
echo "After cloning, ${gitReposFolder} contains:"
ls -1 ${gitReposFolder}
