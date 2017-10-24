@echo off
rem
rem Copy the site/* contents to the learn.openwaterfoundation.org website.
rem - replace all the files on the web with local files
rem - location is learn.openwaterfoundation.org/cdss-learn-statemod-dev

rem Set --dryrun to test before actually doing
set dryrun=""
rem dryrun="--dryrun"
set s3Folder="s3://learn.openwaterfoundation.org/cdss-learn-statemod-dev"

rem Make sure that this batch file is being run from the build-util folder
rem Get current folder, see: https://superuser.com/questions/160702/get-current-folder-name-by-a-dos-command
for %%* in (.) do set curDirName=%%~nx*
if NOT "%curDirName%" == "build-util" (
        echo.
        echo Must run from build-util folder
        echo.
        exit /b 1
)

if "%1%" == "" (
	echo ""
	echo "Usage:  copyToOwfAmazonS3 AmazonConfigProfile"
	echo ""
	echo "Copy the site files to the Amazon S3 static website folder:  %s3Folder%"
	echo ""
	exit /b 0
)

set awsProfile=%1%

rem First build the site so that the "site" folder contains current content.
rem - "mkdocs serve" does not do this

@echo on

cd ..
mkdocs build --clean
cd build-util

rem Now sync the local files up to Amazon S3
rem - apparently can't pass an empty argument so comment out %dryrun%
rem %dryrun%
aws s3 sync ../site %s3Folder% --delete --profile %awsProfile%

exit /b 0
