---
layout: page
title: Quickstart
---

### Amdahl's profiler, directed optimization.
----

"We should forget about small efficiencies, say about 97% of the time: premature optimization is the root of all evil"

 *- Donald Knuth (1974). "Structured Programming with go to Statements". ACM Journal Computing Surveys 6 (4): 268.*

----

Amdahl's profiler organizes profiling output files in a visually appealing way. It is meant to help to balance development vs. execution time by helping to quickly identify the most promising sections of code to optimize and by projecting potential gains it helps you decide whether it is worthwhile to spend time optimizing code.


## Installation 

There is a release on [CRAN](http://cran.r-project.org/web/packages/aprof/index.html),
but to install a more recent developmental version from github go [here](https://github.com/MarcoDVisser/aprof).

## A simple example

This guide has one objective, quickly showcase the major features of aprof. Running through the examples here should take you
no longer than 10 minutes. For a much more in depth tutorial on optimized programming in R, including detailed explanations and code examples for
profiling, parallel computing and re-factoring code in lower-level languages as C, see the tutorial on high performance computing.

Lets start with a very simple example, where the benefits of optimization are obvious, pre-allocation of space v.s. "growing objects".

First copy and paste the following function in R:

```r
# create function to profile
     foo <- function(N){
             preallocate<-numeric(N)
             grow<-NULL
              for(i in 1:N){
                  preallocate[i]<-N/(i+1)
                  grow<-c(grow,N/(i+1))
                 }
            }
```

In this function we essentially have two distinct ways of saving results using 1) pre-allocation (*preallocate[i]<-N/(i+1)*) or 2) growing (*grow<-c(grow,N/(i+1))*).
Next, lets use profiling to find out how much faster the one method over the other is:

### Profiling

```r
# load aprof
require(aprof)
```

Aprof requires a source file in a simple text file (.R or .txt) which we then associate with profiler output. So we first need to save our function:

```r
#save function to a source file and reload
     dump("foo",file="foo.R")
     source("foo.R")
```
Here we saved the file to the hard drive, and named it "foo.R". We then reload the file with "source", as this ensure that the saved file and the source coded loaded in R match up exactly.

Now lets run the R profiler.

```r
     # create file to save profiler output
     tmp<-tempfile()
     
     # Profile the function
     Rprof(tmp,line.profiling=TRUE)
     foo(5e4)
     Rprof(append=FALSE)

### Using aprof
     
Our first step before analysing any output is to create a aprof object. Here we link the profiler output to the source file.
     
```r     
     # Create a aprof object
     fooaprof<-aprof("foo.R",tmp)
```

That's it now we are ready to start our analysis of the function "foo". Basic information can be obtained by printing any aprof object:

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

Using "summary" on an aprof object  gives projections of potential code optimization gains. 

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

Here a table is returned with the theoretical maximal improvement in execution time for the entire program when a given line of code is sped-up.
This basically show you the expected speed-up in execution time when you succeed in improving the execution speed of a single line of code by
the factor S. It also shows the maximum achievable speed-up (at current scaling), here we see that even if we sped-up line 7 infinitely the whole program
would run only 48 times faster. This table gives you all the information you need to decide whether it is worth it to optimize a given line of code.
See ?summary.aprof for more details.

One quick and easy way to visualize bottlenecks in smaller programs is to plot a aprof object:

```r
     plot(fooaprof)

```
![](http://i.imgur.com/lb1UBCI.png)

This is the standard aprof plot. It shows the execution density for each line in a source code file. When the program source code is longer a better way to visualize bottlenecks is with a profile plot:


``` r
    profileplot(fooaprof)
``` 
![](http://i.imgur.com/yFy3fLY.png)

The profile plot, uses the profiler samples to reconstruct the progression through the program lines. The left panel shows the progression through time, while the largest bottleneck is indicated in red. The right panel gives the density of the line calls.

Often a single line of code has so many functions in it that it is hard to identify which function is causing the bottleneck, in these cases one final useful feature is the "targetedSummary" function. This can give you insight in what is slowing down your calculating within a single line of code. Here we apply it on line 7 of the program "foo". 

```r
targetedSummary(fooaprof,target=7,findParent=TRUE)
```

```
Function Parent Calls Time
c   	   L7   168 3.36
```


This will give a detailed summary of the time taken by each function in a given line. In the example, a call to "c" ("combine" function) in line 7, takes most time. When the option "findParent" is set to "TRUE", aprof will attempt to detect any parent functions (functions nested within other functions) and report the parent and child functions. In this case the function c is only nested within the code in L7 (line 7), and has no further parent calls.   


### Another short case study.

The second example in this quickstart guide is one where it is much less clear where the bottlenecks are (to be continued).