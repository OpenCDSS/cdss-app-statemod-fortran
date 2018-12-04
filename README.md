# cdss-app-statemod-fortran #

This repository contains the source code for the StateMod water allocation model,
which is part of [Colorado's Decision Support Systems (CDSS)](http://cdss.state.co.us).
The StateMod software is being migrated to an open source software project as part of the OpenCDSS project.
See the following online resources:

* [OpenCDSS](http://learn.openwaterfoundation.org/cdss-learn-statemod-dev/)
* [StateMod Developer Documentation](http://learn.openwaterfoundation.org/cdss-app-statemod-fortran-doc-dev/)
* [StateMod User Documentation](http://learn.openwaterfoundation.org/cdss-app-statemod-fortran-doc-user/)

The developer documentation and guidelines will be updated as the development environment is used in development.  See the following sections in this page:

* [StateMod Repository Folder Structure](#statemod-repository-folder-structure)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)

-----

## StateMod Repository Folder Structure ##

The following are folders in the repository.  Eclpse Photran has been initially configured but developers often use the command-line compiler tools.  Other development environment tools may be used in the future.

```
cdss-app-statemod-fortran/    StateMod source code and development working files.
  .github/                    Folder used by Git (DO NOT MODIFY THIS).
  .settings/                  Used by Eclipse, may be removed in the future.
  build-util/                 Windows batch files and Linux scripts to build software.
  doc-doxygen-project/        Doxygen project to help understand code logic (experimental).
  src/                        StateMod source code root folder.
    main/                     StateMod main program code.
      fortran/                Folder for Fortran code, makefiles, etc.
  .cproject                   Used by Eclipse PHOTRAN.
  .gitattributes              Standard Git configuration file.
  .gitignore                  Standard Git configuration file.
  .project                    Used by Eclipse.
  LICENSE.md                  StateMod software license (GPL v3).
  README.md                   This file.
```

The following folder structure is recommended for StateMod development.
Top-level folders should be created as necessary.
Repositories are expected to be on the same folder level to allow scripts in those repositories to work.

```
C:\Users\user\                               Windows user home folder.
/home/user/                                  Linux user home folder.
  cdss-dev/                                  Projects that are part of Colorado's Decision Support Systems.
    StateMod/                                StateMod product folder.
      git-repos/                             Git repositories for StateMod.
        cdss-app-statemod-fortran/           StateMod source code development.
        cdss-app-statemod-fortran-doc-dev/   StateMod develpoer documentation.
        cdss-app-statemod-fortran-doc-user/  StateMod user documentation.
        cdss-app-statemod-fortran-test/      StateMod automated tests.
```

## Contributing ##

Contributions to this project can be submitted using the following options:

1. StateMod software developers with commit privileges can write to this repository
as per normal OpenCDSS development protocols.
2. Post an issue on GitHub with suggested change (preferred for small changes).  Provide information using the issue template.
3. Email a development contact.
4. Fork the repository, make changes, and do a pull request (preferred for large changes).
Contents of the current master branch should be merged with the fork to minimize
code review before committing the pull request.

See also the [OpenCDSS / StateMod protocols](http://learn.openwaterfoundation.org/cdss-website-opencdss/statemod/statemod/).

## License ##

The software is licensed under GPL v3+.  See the [LICENSE.md](LICENSE.md) file.

## Contact ##

See the [OpenCDSS / StateMod leadership information for contacts](http://learn.openwaterfoundation.org/cdss-website-opencdss/statemod/statemod/#product-leadership).
