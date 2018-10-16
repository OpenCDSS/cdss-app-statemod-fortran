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

version="1.2.0 2018-10-16"

# Parse the command parameters
while getopts :hm:p:u:v opt; do
	#echo "Command line option is ${opt}"
	case $opt in
		h) # usage
			echo ""
			echo "Usage:  git-clone-all.sh -m mainRepo -p productHome -u remoteRootUrl"
			echo ""
			echo "    git-clone-all.sh -m main-repo-folder -p dev-folder/ProductName -u https://github.com/SomeAccount"
			echo "    git-clone-all.sh -m cdss-app-tstool-main -p cdss-dev/TSTool -u https://github.com/OpenCDSS"
			echo "         -m specifies the main repository name."
			echo "         -p specifies the product home folder relative to user's home folder"
			echo "            (git-repos folder is assumed to exist under this)."
			echo "         -u specifies the root URL where repositories will be found."
			echo ""
			echo "    -h prints the usage"
			echo "    -v prints the version"
			echo ""
			exit 0
			;;
		m) # main repo
			mainRepo=$OPTARG
			;;
		p) # product home
			productHome=$OPTARG
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

# Main script entry point

# Product development main folder
# - top-level folder for the product
home2=$HOME
if [ "${operatingSystem}" = "cygwin" ]
	then
	# Expect product files to be in Windows user files location (/cygdrive/...), not Cygwin user files (/home/...)
	home2="/cygdrive/C/Users/$USER"
fi
if [ -z "${remoteRootUrl}" ]
	then
	echo ""
	echo "GitHub root URL has not been specified.  Exiting."
	exit 1
fi
if [ -z "${mainRepo}" ]
	then
	echo ""
	echo "Main repository has not been specified.  Exiting."
	exit 1
fi
if [ -z "${productHome}" ]
	then
	echo ""
	echo "Product home folder has not been specified.  Exiting."
	exit 1
fi
# The product home is relative to the user's files in a standard development files location:
# 
# $HOME/
#    DevFiles/
#      ProductHome/
#        git-repoos/
#          repo-name1/
#          repo-name2/
#          ...
productHomeAbs="$home2/${productHome}"
# Git repos are located in the following
gitReposFolder="${productHomeAbs}/git-repos"
# Main repository in a group of repositories for a product
# - this is where the product repository list file will live
mainRepoFolder="${productHomeAbs}/git-repos/${mainRepo}"
gitRepoFolder=`dirname ${mainRepoFolder}`
# The following is a list of repositories including the main repository
# - one repo per line, no URL, just the repo name
# - repositories must have previously been cloned to local files
repoListFile="${mainRepoFolder}/build-util/product-repo-list.txt"

# Make sure that expected folders exist
if [ ! -d "${productHomeAbs}" ]
	then
	echo ""
	echo "Product folder \"${productHomeAbs}\" does not exist.  Exiting."
	exit 1
fi
if [ ! -d "${gitReposFolder}" ]
	then
	echo ""
	echo "Product git-repos folder \"${gitReposFolder}\" does not exist.  Exiting."
	exit 1
fi
if [ ! -d "${mainRepoFolder}" ]
	then
	echo ""
	echo "Main repo folder \"${mainRepoFolder}\" does not exist.  Exiting."
	exit 1
fi
if [ ! -f "${repoListFile}" ]
	then
	echo ""
	echo "Product repo list file \"${repoListFile}\" does not exist.  Exiting."
	exit 1
fi

while [ "1" = "1" ]
do
	echo ""
	echo "Clone all repositories for the product, to set up a new developer environment."
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
	repoFolder="${productHomeAbs}/git-repos/${repoName}"
	repoUrl="${remoteRootUrl}/${repoName}"
	echo "================================================================================"
	echo "Cloning repo:  ${repoName}"
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
			echo "Dry run:  cloning repo with:  git clone \"${repoUrl}\""
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
