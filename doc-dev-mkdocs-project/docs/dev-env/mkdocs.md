# Development Environment / MkDocs #

MkDocs is a Python add-on package that creates static website content from Markdown input.
This documentation was created with MkDocs.
Although the latest published version of this documentation can be viewed on the web,
MkDocs must be installed and run locally if the documentation is to be viewed locally
and changes to documentation are made by developers.
See the installation instructions on the [MkDocs website](http://www.mkdocs.org/),
which are summarized below.
MkDocs installation can be skipped if the developer will never look at the developer documentation locally
or make edits to the documentation.

This documentation includes the following sections:

* [Prerequisites](#prerequisites)
* [Install MkDocs](#install-mkdocs)
	+ ![Linux](../../images/linux-32.png) [Linux](#linux)
	+ ![Windows](../../images/windows-32.ico) [Windows](#windows)

----------------------

## Prerequisites ##

Before installing MkDocs, it is necessary that [Python is installed](python) and [`pip` is installed](pip).

## Install MkDocs ##

### ![Linux](../../images/linux-32.png) Linux ###

**TODO smalers 2016-12-31 Linux documenation will be completed after Windows version**

### ![Windows](../../images/windows-32.ico) Windows ###

To install MkDocs, open a Windows Command Shell and run the following.
This assumes that Python was installed and Python environment variables were set.

To check whether MkDocs is installed:


```
py -m mkdocs
```

If not installed, install with the Python 3 version (actually no need to specify `-3` but do to illustrate installing for specific Python version):

```
py -3 -m pip install mkdocs
```

Check whether installed by running:

```
py -m mkdocs 
```

or simply:

```
mkdocs
```

If installed, the usage will be shown, similar to:

```
Usage: mkdocs [OPTIONS] COMMAND [ARGS]...

MkDocs - Project documentation with Markdown.

Options:
    -V, --version  Show the version and exit.
    -q, --quiet    Silence warnings
    -v, --verbose  Enable verbose output
    -h, --help     Show this message and exit.

Commands:
  build      Build the MkDocs documentation
  gh-deploy  Deploy your documentation to GitHub Pages
  json       Build the MkDocs documentation to JSON files...
  new        Create a new MkDocs project
  serve      Run the builtin development server

```

See the [Development Tasks / Documenting](../dev-tasks/documenting#developer-documentation-using-mkdocs) documentation for instructions on using MkDocs.
