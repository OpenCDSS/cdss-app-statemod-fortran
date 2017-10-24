# Development Tasks / Version Control with Git #

**TODO smalers 2017-10-24 this section needs to be reworked now that the CDSS Git training documentation is available.**

The OpenCDSS project is using Git and GitHub for version control.
Git protocols for StateMod follow OpenCDSS protocols and are summarized here for reference.
Resources for Git include:

* [Installing Git for StateMod](../dev-env/git) - walk through of Git installation
* [Pro Git](https://git-scm.com/book/en/v2) - useful Git reference with examples
* [Udacity: How to Use Git and GitHub](https://www.udacity.com/course/how-to-use-git-and-github--ud775) - useful free online video course
* [CDSS / Learn Git](http://learn.openwaterfoundation.org/cdss-learn-git/) - CDSS reference for Git

This page includes the following sections to summarize common Git/GitHub tasks.

* [CDSS Git Workflow Overview](#cdss-git-workflow-overview)
* [Forking the Repository](#forking-the-repository)
* [Cloning the Repository](#cloning-the-repository)
* [Checking Repository Status](#checking-repository-status)
* [Pulling Changes from the Remote Repository](#pulling-changes-from-the-remote-repository)
* [Creating a Branch](#creating-a-branch)
* [Checking out a Branch](#checking-out-a-branch)
* [Committing Changes to a Branch](#committing-changes-to-a-branch)
* [Merging a Branch](#merging-a-branch)
* [Pushing Changes to the Remote Repository](#pushing-changes-to-the-remote-repository)
* [Deleting a Branch](#deleting-a-branch)

--------------

## CDSS Git Workflow Overview ##

"Git workflow" (and more generically "versioning workflow") refers to the protocols
by which file versions are maintained during software development.
The Git workflow includes protocols for the software project, code committers, and individual developers.
The Git workflow boils down to how to handle individual and grouped file edits and the use of branches
to manage such change sets.

The "feature branch workflow" selected for OpenCDSS software projects reflects the
fact that software developers may not utilize Git/GitHub
on a frequent basis and the small development team needs to be nimble.
Therefore the workflow that has been chosen is a "feature branch" approach as illustrated in the
following graphic.  This is in contrast to the popular ["A successful Git branching model" workflow](http://nvie.com/posts/a-successful-git-branching-model/),
which relies on a "dev" branch for development.

![Git feature branch workflow](versioning-images/git-feature-workflow.png).

Important characteristics of the feature branch workflow are:

* the "master" branch is main branch, which is straightforward because Git tools default to that branch 
* new branches are created for new features or bug fixes
	+ for example branches can reflect the issue number from GitHub
	+ 1-feature-short-name
	+ 2-bug-short-name
	+ test-some-test
* developers can pull from the master to make sure the branch reflects changes to the master
* feature branches can be merged by committers
* software releases can be tagged on the master branch, and, if necessary, stable version branches can be created and maintained

The above workflow is assumed for the remainder of the documentation.

## Forking the Repository ##

GitHub provides features to "fork" a repository.
The fork is a cloud-hosted copy of the original repository with both hosted on GitHub.
This is needed when a software developer does not have write permissions to the main repository and
needs to have a cloud-hosted copy of the repository to allow maintainers of the main repository to pull changes.
The developer can then do a "pull request" to alert developers of the main repository to incorporate changes from the fork.

Software developers that have write access to the main GitHub repository do not need to fork and instead
can directly clone the repository, as discussed below.

***TODO smalers 2017-08-27 need to fill out fork and pull request example***

## Cloning the Repository ##

A clone is a local copy of a repository and is needed to ensure that each developer can work on their own copy of a repository.
A GitHub repository that is visible to a software developer (public repository or developer has been given read and/or write permissions)
can be directly cloned.
This is particularly useful for developers that have write permissions to the main repository on GitHub.
If a fork is used (see previous section), then the forked copy is cloned to the local machine.

A clone may be created for the following reasons:

* [set up a new development environment](../dev-env/overview) to view and/or make changes to the code
* test development environment documentation as if setting up a new development environment
* create a temporary copy during automated testing to automate compiling, testing, and building releases

The StateMod repository can be cloned as shown below, using the suggested folder structure.
The path to the repository can be copied from GitHub using the "Clone or download" button on the main repository page.

See also the [git clone documentation](https://git-scm.com/docs/git-clone).

The following command would be run in the folder where the local Git repositories reside.
Subsequent `git` commands can be run in the repository working files.


```sh
git clone https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran.git

```

Once cloned, the `cdss-app-statemod-fortran` folder will contain a working copy of repository files.
The `.git` folder (hidden on Linux) contains the repository contents as per Git software internal conventions
and should not generally be modified in any way.

Once the repository has been cloned, the StateMod software and other files can be compiled, run, and modified
as per development protocols.

## Checking Repository Status ##

It is important to understand the status of working files...have any files been added, deleted, or modified?

Interactive development tools tend to show the repository status in real-time, based on file changes.
For example, in Eclipse, files that are modified are shown with a `>` symbol.

The `git status` command shows a status of files (see [git status documentation](https://git-scm.com/docs/git-status)).
The following example illustrates changes that have occurred in editing this documentation:

```sh
sam (1-cleanup-doc *) dev-tasks $ git status
On branch 1-cleanup-doc
Changes not staged for commit:
  (use "git add/rm <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   ../../../README.md
        modified:   ../StateMod-favicon.png
        modified:   ../dev-env/git.md
        modified:   versioning.md
        deleted:    ../images/CDSS-website.png
        modified:   ../index.md
        modified:   ../../mkdocs.yml

Untracked files:
  (use "git add <file>..." to include in what will be committed)

        ../../README.md
        versioning-images/
        ../index-images/

no changes added to commit (use "git add" and/or "git commit -a")
```

Note that the prompt reflects use of the Git command prompt utilities listed in resources at the top of this page.
In this case the Git bash user name is shown "sam" and repository branch "1-cleanup-doc".
The asterisk indicates that changes have occurred.

The branch name is also output as `On branch 1-cleanup-doc`.
Git commands show files relative to the folder where the command was run.

In order to commit changes of any kind to the repository it is necessary to add them.
See [Committing Changes to a Branch](#committing-changes-to-a-branch) below.

## Pulling Changes from the Remote Repository ##

Git and GitHub are distributed version control systems.
This means that developers at different locations each have a complete version of the repository, to a degree.
When a repository is cloned, the list of remote branches are cloned to the local repository and the active
branch in the repository is created and checked out.
If the master branch is the active branch, then the local copy of the repository will have the history of
the master branch and a list of the other remote branches.

A simple case is that a developer that is collaborating with other developers will use the following workflow:

1. Clone the repository (master branch is the active branch)
2. Some amount of time passes during which time other developers may have committed changes to the master.
3. Before creating a new local branch, pull the latest contents of the remote repository.
4. Create a new branch and check it out so that it active for work.
5. Make changes to the code and commit to the branch (may also do additional pulls from master to keep current).
6. If the developer has commit permissions, merge the branch into the master.
7. Push the master to the remote repository.
8. Delete the local branch after the merge since no longer needed.

In this case it is important to pull from the remote master branch so that local master branch is current
and consistent as much as possible, other than any changes made locally.

Use the `git pull` command (see [git pull documentation](https://git-scm.com/docs/git-pull))
to fetch and merge changes from the master into the local master,
assuming the current branch is master:

```sh
git pull
```

Note that it may be desirable to fetch changes from the repository but not merge into the local branch.
This can be accomplished by using the `git fetch` command and then `git merge`.
This is a bit more advanced and is discussed below.

## Creating a Branch ##

New changes should normally occur in a branch of the repository.
As discussed elsewhere in this documentation,
a branch naming convention that is useful is to use the GitHub issue number with issue type
(e.g., `bug`, `feature`) and a short topic, separated by dashes.
The issue type may not be needed but is helpful to understand whether changes are an enhancement or bug fix.
Do not use slashes to separate branches as is indicated in some Git documentation because it causes technical issues.
For example, if a bug is being fixed, then a branch could be created as follows (see the [git branch documentation](https://git-scm.com/docs/git-branch):

```sh
git branch 3-bug-input-file-comments
```
The above creates a branch but does not check out the files for that branch.

Note that making changes on the master branch working files is OK because
`git branch` can be executed at a later time to ensure that changes are committed for that branch.
However, it is generally recommended to create the branch before making changes, if possible.

## Checking out a Branch ##

Once a branch has been created (see previous section), the branch needs to be checked out in order to
copy the branch's files to the working files.
If a new branch has been created, then the branch will contain the files of the parent branch.
Any files that were modified before checking out the branch will exist in the branch working files so they can be committed.
See the [`git checkout` documentation](https://git-scm.com/docs/git-checkout).

To check out a branch, for example:

```sh
git checkout 3-bug-input-file-comments
```

The working files can then be edited.

## Committing Changes to a Branch ##

Files that are modified in any way are only changed in the working files and must be committed to the local repository
before further actions can occur.  Working files are committed to "the index", also known as the "staging area".
The files in the staging area are then committed.  See the [`git add`](https://git-scm.com/docs/git-add),
[`git rm`](https://git-scm.com/docs/git-rm), and
[`git mv`](https://git-scm.com/docs/git-mv) documentation.

Every file that has been added/deleted/modified can be added with a single `git add` command
(note that old versions of Git only processed the current and child folders):

```sh
git add -A
```

The above command has the disadvantage of possibly adding files that should not be committed,
for example files that should be added to `.gitignore` file.
To remove a file that was added by mistake, use `git checkout -- fileName`.

To add a single file or group of files using `*` as wildcard:

```sh
git add path-to-file
```

To delete a single file or group of files using `*` as wildcard:

```sh
git rm path-to-file
```

To move a single file or group of files using `*` as wildcard:

```sh
git mv path-to-file
```

Specify the path to file using relative path as output by `git status`.

After all files of interest have been added to the staging area, commit and specify a message:

```sh
git commit -m 'Commit message'
```

If the commit message is not specified, a text file editor will be used to enter the commit message.

## Merging a Branch ##

In order to merge a branch's committed changes, the receiving branch should first be checked out,
and then the branch's contents are merged.
By default, Git will use a "fast forward" merge, meaning that all changes are "replayed" onto the receiving branch as if they
were made in sequence.
This has the issue that if multiple branches are merged, it is not possible to see where the branch as a whole ended.
To address this, use the `git merge --no-ff branchName` command, which will automatically insert an extra commit
to delineate the branch.

```sh
git checkout master
git merge --no-ff 3-bug-input-file-comments
```

The above commands will ensure that the branch `3-bug-input-file-comments` is merged into the master.
The editor that has been specified will then be used to enter the commit message for the merge,
which is an additional commit beyond the specific file commits.

## Pushing Changes to the Remote Repository ##

Once changes to the local master have been made, the changes can be pushed to the remote GitHub repository:

```sh
git push
```

You will be asked to enter the GitHub account information to match an account with write permissions.

The process can then start again.

To confirm that the changes have been made, review the repository source files on the GitHub website.

## Deleting a Branch ##

Once a short-lived branch has been merged, there is no need to keep it around.
To delete a branch:

```sh
git branch -d 3-bug-input-file-comments
```
