# cdss-app-statemod-fortran

This repository contains the source code for the StateMod water allocation model,
which is part of [Colorado's Decision Support Systems (CDSS)](http://cdss.state.co.us).
CDSS is supported by the [Colorado Water Conservation Board](http://cwcb.state.co.us) and
[Colorado Division of Water Resources](http://water.state.co.us).

The StateMod software is being migrated to an open source software project as part of the OpenCDSS project
led by the [Open Water Foundation](http://openwaterfoundation.org).

See the following online developer documentation to get started as a StateMod developer:

* [Learn StateMod (for Developers)](http://learn.openwaterfoundation.org/owf-learn-cdss-statemod-dev/index.html)

The developer documentation and guidelines will be updated as the development environment is proven out.

* [StateMod Repository Folder Structure](#statemod-repository-folder-structure)
* [Cloning this Repository](#cloning-this-repository)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)

-----

<a name="statemod-repository-folder-structure"></a>
## StateMod Repository Folder Structure ##

The following folder structure is recommended for StateMod development including this documentation.
Top-level folders should be created as necessary.

### Linux ###

```
/home/user/                                 (user's home folder)
    cdss-dev/                               (work done for Colorado's Decision Support Systems)
        StateMod/                           (work related to the StateMod product)
           git-repos/                       (Git repositories for StateMod)
               cdss-app-statemod-fortran/   (the StateMod code and documentation repository)
```

### Windows ####

```
C:\Users\user\                              (user's home folder)
    cdss-dev\                               (work done for Colorado's Decision Support Systems)
        StateMod\                           (work related to the StateMod product)
           git-repos\                       (Git repositories for StateMod)
               cdss-app-statemod-fortran\   (the StateMod code and documentation repository)
```

<a name="cloning-this-repository"></a>
## Cloning this Repository ##

Clone this repository using a Git software client, for example using Git command line:

### Linux ###

```sh
> cd /home/user/cdss-dev/StateMod/git-repos
> git clone https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran.git
```

### Windows ###

```sh
> C:
> cd \Users\user\cdss-dev\StateMod\git-repos
> git clone https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran.git
```

<a name="contributing"></a>
## Contributing ##

Contributions to this project can be submitted using the following options:

1. StateMod software developers with commit privileges can write to this repository
as per normal CDSS development protocols.
2. Post an issue on GitHub with suggested change (preferred for small changes).
3. Email the contact.
4. Fork the repository, make changes, and do a pull request (preferred for large changes).
Contents of the current master branch should be merged with the fork to minimize
code review before committing the pull request.

<a name="license"></a>
## License ##

A license for the software is being determined as part of the OpenCDSS project.

<a name="contact"></a>
## Contact ##

The lead developers/maintainers for StateMod are:

* Steve Malers, Open Water Foundation, [steve.malers@openwaterfoundation.org](mailto:steve.malers@openwaterfoundation.org)
(initial author of this developer documentation and architect of OpenCDSS development/test environment)
* Ray Bennett, original StateMod developer and current lead contributor
* Andy Moore, Colorado Water Conservation Board, [andy.moore@state.co.us](mailto:andy.moore@state.co.us) (CDSS lead at the CWCB)
