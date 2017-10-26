# Initial Project Setup / Create Development Home Folder #

See the [discussion of development project folder structure](overview#development-folder-structure).

The following sections are included in this documentation:

* [Prerequisites](#prerequisites)
* [Create Home Folder for Development Files](#create-home-folder-for-development-files)
	+ ![Linux](../images/linux-32.png) [Linux](#linux)
	+ ![Windows](../images/windows-32.ico) [Windows](#windows)

----------

## Prerequisites ##

All setup for the project should occur as a normal user (not root, administrator, or other special user) in the software developer's home folder.
Most of the contents of this folder will be backed up during GitHub repository commits
but can also be backed up if the user's files are backed up normally.
Additional backups can be made by periodically copying files to a backup device or configuring automated backups of the folder.

## Create Home Folder for Development Files ##

Assuming that the folder structure is as documented for the project folder structure,
create a folder to serve as an umbrella for the StateMod development.
This folder may have already been created as part of software tool installation, for example to run scripts for installed software.

Additional folders will be created as specific software tools are installed and configured,
and software files are added, as per other documentation sections.
If documentation refers to a folder that has not been created, it is generally OK to create it manually
because few if any of the top-level folders are created with scripts.

### ![Linux](../images/linux-32.png) Linux ###

Execute the following commands in a shell such as `bash`.

```sh
$
$ cd
$ # Create a folder for all CDSS development projects, to differentiate from other user files
$ mkdir cdss-dev
$ cd cdss-dev
$ # Create a folder for the StateMod software project development files
$ mkdir StateMod
```

### ![Windows](../images/windows-32.ico) Windows ###

Execute the following commands in a Windows command prompt window, or use File Explorer to create folders,
or perform the equivalent steps in a Git Bash window.

```com
>
> C:
> cd \Users\theUser
> # Create a folder for all CDSS development projects, to differentiate from other user files
> mkdir cdss-dev
> cd cdss-dev
> # Create a folder for the NovaStar REST web services software project development files
> mkdir StateMod
```
