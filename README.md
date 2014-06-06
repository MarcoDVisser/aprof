aprof (0.21)
=====

Amdahl's profiler, directed optimization.

Assists the evaluation of whether and where to focus code optimization, using [Amdahl's law](https://en.wikipedia.org/wiki/Amdahl%27s_law) and visual aids based on line profiling. Amdahl's profiler organises profiling output files (will soon include memory profiling) in a visually appealing way. It is meant to help to balance development vs. execution time by helping to identify the most promising sections of code to optimize and projecting potential gains. The package is an addition to R's standard profiling tools and is not a wrapper for them.

## Installation

There is a release on [CRAN](http://cran.r-project.org/web/packages/aprof/index.html),
but to install a more recent developmental version from github you can download the most recent version as [zip](https://github.com/MarcoDVisser/choosecolor/zipball/master) 
or [tar ball](https://github.com/MarcoDVisser/choosecolor/tarball/master).
To install decompress these and run R CMD INSTALL on the contents of the archives, or use the **devtools** package to install the current development version from R.


```r
## devtools is required
require(devtools)
install_github("aprof", "MarcoDVisser")
```

## Dependencies

aprof is meant to be light and has no other dependencies other than the base R installation.

## Example
```r
require(aprof)
# create function to profile
     foo <- function(N){
             preallocate<-numeric(N)
             grow<-NULL
              for(i in 1:N){
                  preallocate[i]<-N/(i+1)
                  grow<-c(grow,N/(i+1))
                 }
            }
     
     #save function to a source file and reload
     dump("foo",file="foo.R")
     source("foo.R")
     
     # create file to save profiler output
     tmp<-tempfile()
     
     # Profile the function
     Rprof(tmp,line.profiling=TRUE)
     foo(5e4)
     Rprof(append=FALSE)
     
     # Create a aprof object
     fooaprof<-aprof("foo.R",tmp)
     plot(fooaprof)
```
## Examples of output
The standard aprof plot. It shows the execution density for each
line in a source code file.
![](http://i.imgur.com/lb1UBCI.png)

``` r
    # From above example:
    # Create a aprof object
    fooaprof<-aprof("foo.R",tmp)
    profileplot(fooaprof)
``` 

A profile plot, which can be used for large source files. It shows the largest bottlenecks in red.
![](http://i.imgur.com/yFy3fLY.png)

## Installation
Special thanks to Dason Tyler Rinker, Dason Kurkiewicz and Diego Mayer-Cantu for comments, commits and additions to this package.


