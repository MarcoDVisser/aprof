aprof
=====

Amdahl's profiler, directed optimization.

A R package meant to help evaluate whether and where to focus 
code optimization using [Amdahl's law](https://en.wikipedia.org/wiki/Amdahl%27s_law).

## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/),
but you can download the [zip](https://github.com/MarcoDVisser/choosecolor/zipball/master) 
or [tar ball](https://github.com/MarcoDVisser/choosecolor/tarball/master).
To install decompress these and run `R CMD INSTALL` on the conents of the
achives, or use the **devtools** package to install the current 
development version.


```r
## devtools is required
require(devtools)
install_github("aprof", "MarcoDVisser")
```

## Dependancies 

aprof has no other dependancies other than the base R installation.
