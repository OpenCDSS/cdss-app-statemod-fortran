# Development Tasks / Coding #

Fortran is a powerful language that has been in use for many years.
However, other languages are more popular and mainstream.
Consequently, understanding Fortran may be a challenge.

See the following resources:

* [GNU Fortran Compiler documentation](https://gcc.gnu.org/onlinedocs/gfortran/index.html)
* [Modernizing old Fortran](http://fortranwiki.org/fortran/show/Modernizing+Old+Fortran)
* [Fortran 90 Best Practices](http://www.fortran90.org/src/best-practices.html)
* [Professional Fortran 77](http://www.star.le.ac.uk/~cgp/prof77.html)

The remainder of this page contains the following sections:

* [Troubleshooting Old Fortran Code](#troubleshooting-old-fortran-code)

---------------

## Troubleshooting Old Fortran Code ##

The StateMod code has been around for many years and has evolved through different versions of Fortran and compilers.
Most recently, the code has been updated to compile on `gfortran` compiler, which supports Fortran 90/95+ language.
This has led to the discovery of a number of inherent code problems that need to be resolved to produce the same results.
The following are issues that have been discovered.

### `IF` StateMents with Multiple Clauses all Evaluate in `gfortran`, not Left to Right ###

The Fortran standard specifies `IF` statements containing multiple parts will result in all parts being evaluated
and the order of evaluation is not specified.
Consequently, the programmer cannot rely on a left-to-right evaluation, such as for the C language.
Apparently the Lahey compiler does assume left-to-right evaluation and will exit the evaluation when
a required level of logical evaluation has resulted.
This has in particular been an issue with `gfortran` version where the evaluation of the clauses results
in evaluating array indices that are out of bounds, such as from having initial values of `0` or `-1`.
See the following before and after code snippets from the
[`divcar1.for`](https://github.com/OpenWaterFoundation/cdss-app-statemod-fortran/blob/master/src/main/fortran/divcar1.for)
routine:

Before:

```
IF(iresw.eq.0.and.IRTURN(IUSE).EQ.4) GO TO 360
```
After:

```
if(iresw.eq.0) then
  if(IRTURN(IUSE).EQ.4) then
    GO TO 360
  endif
endif
```

The above will evaluate properly on Lahey and `gfortran`, but may actually need a further check to confirm that
`IUSE` is >= 1.

Invalid array bounds result in a `gfortran` program stopping with an error such as the following
(different code from above).
Array indices should be >= 1 and less than the array dimension (unless the dimension range was set differently
when declaring the array).


```
  Mdainp.for
  + Execut; Year  1908 Month OCT  Day   1 Reoperation     1 Annual Total     0
  + Execut; Year  1908 Month OCT  Day   1 Reoperation     2 Annual Total     1
  + Execut; Year  1908 Month OCT  Day   1 Reoperation     3 Annual Total     2
  At line 522 of file divresp2.for
  Fortran runtime error: Index '0' of dimension 1 of array 'iplntyp' below lower bound of 1
```
