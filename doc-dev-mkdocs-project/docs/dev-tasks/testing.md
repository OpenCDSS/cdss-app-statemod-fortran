# Development Tasks / Testing #

Automating software testing is an important software development task because it helps ensure that software performs as intended.
It is particular important to ensure that a change in one part of the code does not break another part of the code,
and that changes in the computer environment don't break the software.
Automated testing has not been a large part of StateMod software development in the past and needs to be added.

This documentation contains the following sections:

* [Test Code Using Unit Tests](#test-code-using-unit-tests)
* [Test StateMod by Running Reference Dataset](#test-statemod-by-running-reference-dataset)
* [Automated Functional Testing Using `pytest`](#automated-functional-testing-using-pytest)
* [Automated Functional Testing Using TSTool](#automated-functional-testing-using-tstool)

----------

## Test Code Using Unit Tests ##

Unit tests are compiled code the test an atomic unit of code.
Unit tests are a tool for software developers that require access to the develop environment,
unlike functional tests, that could be run by users without the full developer environment.

There does not seem to be extensive unit test frameworks for Fortran.  See:

* [A look at FORTRAN unit test frameworks](https://www.software.ac.uk/blog/2016-09-28-look-fortran-unit-test-frameworks)

A place-holder folder `src/test` has been created in the repository and `src/test/fortran` could be used for unit tests.

**TODO smalers 2017-01-02 need to evaluate Fortran unit tests for automated testing - how does it fit into this project.**

## Test StateMod by Running Reference Dataset ##

**TODO smalers 2017-01-10 need WWG to help explain which dataset should be run and how to check results.
Need to put the dataset somewhere on the web for developers to download.
Perhaps also explain how to use tools like KDiff3.**

## Automated Functional Testing Using pytest ##

A place-holder folder `src/test` has been created in the repository and `src/test/pytest` could be used for `pytest` tests.

**TODO smalers 2017-01-10 pytest is envisioned as a way to run functional tests, for example run statemod from the command line
on simple datasets and compare output with expected output.  Need to decide whether to prototype automated testing with pytest or
try some other automation tool.**

## Automated Functional Testing Using TSTool ##

TSTool software can be used to implement automated tests and this approach is used to test TSTool itself.
The benefit of using TSTool is that it can read StateMod input and output time series files and can do time series 
comparison using a precision.
Therefore if small, allowable, differences are seen, TSTool can treat as a pass,
whereas a simple file comparison would show a fail.

A place-holder folder `src/test` has been created in the repository and `src/test/tstool` could be used for TSTool tests.

**TODO smalers 2017-01-10 need to decide whether to prototype this approach.**
