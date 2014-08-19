aprof (0.2.1)
=====

Amdahl's profiler, directed optimization.

Assists the evaluation of whether and where to focus code optimization, using [Amdahl's law](https://en.wikipedia.org/wiki/Amdahl%27s_law) and visual aids based on line profiling. Amdahl's profiler organises profiling output files (will soon include memory profiling) in a visually appealing way. It is meant to help to balance development vs. execution time by helping to identify the most promising sections of code to optimize and projecting potential gains. The package is an addition to R's standard profiling tools and is not a wrapper for them.

## Installation

There is a release on [CRAN](http://cran.r-project.org/web/packages/aprof/index.html),
but to install a more recent developmental version from github you can download the most recent version as [zip](https://github.com/MarcoDVisser/aprof/zipball/master) 
or [tar ball](https://github.com/MarcoDVisser/aprof/tarball/master).
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

A profile plot, which can be used for large source files. It uses the profiler samples to attempt to reconstruct the progression through the program lines. The left panel shows the progression through time, while the largest bottleneck is indicated in red. The right panel gives the density of the line calls.

![](http://i.imgur.com/yFy3fLY.png)


Printing any aprof object will return basic information:
```r
fooaprof
```

```
Source file:
foo.R (9 lines).

 Call Density and Execution time per line number:

      Line  Call Density  Time Density (s)
[1,]  7     282           5.64            
[2,]  6     4             0.08            

 Totals:
 Calls		 287 
 Time (s)	 5.76 	(interval = 	 0.02 (s))
```

Using "summary" gives projections of potential code optimization gains. A table is returned with the theoretical maximal improvement in execution time for the entire profiled program when a given line of code is sped-up. See ?summary.aprof for more details.

```r
summary(fooaprof)
```
```

Largest attainable speed-up factor for the entire program

        when 1 line is sped-up with factor (S): 

	 Speed up factor (S) of a line 
            1      2      4      8      16     S -> Inf**
Line*: 7 :   1.00   1.96   3.76   6.98  12.19  48.00     
Line*: 6 :   1.00   1.01   1.01   1.01   1.01   1.01     

Lowest attainable execution time for the entire program when

             lines are sped-up with factor (S):

	 Speed up factor (S) of a line  
            1      2      4      8      16   
All lines   5.760  2.880  1.440  0.720  0.360
Line*: 7 :  5.760  2.940  1.530  0.825  0.472
Line*: 6 :  5.760  5.720  5.700  5.690  5.685

    Total sampling time:  5.76  seconds
 *  Expected improvement at current scaling
 ** Asymtotic max. improvement at current scaling
```
One useful feature is the "targetedSummary" function. This will give a detailed summary of the time taken by each function in a given line. In the example, a call to "c" ("combine" function) in line 7, takes most time. When the option "findParent" is set to "TRUE", aprof will attempt to detect any parent functions (functions nested within other functions) and report the parent and child functions. In this case the function c is only nested within the code in L7 (line 7), and has no further parent calls.   

```r
targetedSummary(fooaprof,target=7,findParent=TRUE)
```

```
Function Parent Calls Time
c   	   L7   168 3.36
```

## Thanks
Special thanks to Tyler Rinker, Dason Kurkiewicz and Diego Mayer-Cantu for comments, commits and additions to this package. Sean M. McMahon, Cory Merow, Philip Dixon, Sydne Record and Eelke Jongejans thanks for all the suggestions, comments and testing while I was developing this package.

