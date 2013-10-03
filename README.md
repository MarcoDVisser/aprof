aprof
=====

Amdahl's profiler, directed optimization.

An R package meant to help evaluate whether and where to focus 
code optimization using [Amdahl's law](https://en.wikipedia.org/wiki/Amdahl%27s_law) and visual aids based on line profiling. Aprof is an addition to R's standard profiling tools and is not a wrapper for them. Amdahl's profiler organises profiling output files (including memory profiling) in a visually appealing way and helps identify the most promising sections of code to optimize. It is meant to help balance development v.s. execution time. 

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

aprof is meant to be light and has no other dependancies other than the base R installation.

```r
## Examples
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
     foo(1e4)
     Rprof(append=FALSE)
     
     # Create a aprof object
     fooaprof<-aprof("foo.R",tmp)
     plot(fooaprof)
```

![](https://raw.github.com/MarcoDVisser/DDMDD/master/images/DDMDD.png)