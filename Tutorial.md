---
layout: page
title: Tutorial - High performance computing for Biologists
---

# Introduction (under construction as of 19-08-2014)

The use and advancement of mathematical and computational tools in
ecology has seen considerable headway since its pioneering days
\citep{Lotka1925,Volterra1926,Gause1934}. Today, computation and
computer code are fundamental parts of our science and the computational
needs of biologists are extensive (Fig. [fig:WOS]); emerging fields such
as ecoinformatics and computational ecology
\citep{Michener2012,Petrovskii2012} bear witness to the fact that
biologists are collecting and distributing data on larger scales than
ever before \citep[e.g. GenBank, NEON; ][]{Jones2006, Kelling2009} Such
data sources are used to inform, validate and construct ever more
elaborate models \citep[e.g.][]{Clark2010,Nathan2011,Luo2011}. These
complex models are often analytically intractable and require intensive
numerical calculations \citep[e.g.][]{Bohrer2009}. In such an intense
setting, computational issues should not limit the scope of the science
conducted. That is why we decided to compile this guide for improving
computational efficiency in a wide variety of settings. Here, we review
of the scattered information on computational efficiency found online,
in text books and other sources, and provide an overview which includes
some basic theory from computer science, but is mostly a hands-on
tutorial for learning how to identify bottlenecks and speed-up the
corresponding R code. We hope to show that fast and efficient code is
often straightforward and easily implemented.

[!ht]

![image](figure/WoSplot)

[fig:WOS]

Scope of the document
---------------------

A range of computational constraints can be found in a single biological
analysis, let alone across a research program. Constraints might include
the difficulty in performing routine operations on very large datasets,
such as regression analyses across large spatial domains
\citep{Beale2010}, large matrix operations used for population
projection models \citep{Zuidema2010}, optimizing likelihood functions
with complex functional forms \citep{Putten2012}, running Markov Chain
Monte Carlo models for any variety of hierarchical Bayesian models
\citep{Comita2010}, or estimating large phylogenetic trees
\citep{Ropelewski2010}. Some of these challenges are due to analytic
complexity, some to data extent, and some to the scope of the analysis
(e.g. spatial detail or projections over evolutionary time). In this
tutorial, we focus on problems involving routine operations on large
datasets, or common practices used in stochastic simulation models. In
all these examples we give detailed descriptions of the various
techniques discussed in our review article "Speeding up ecological
computations in R" \citet{VisserInprep}.

The widespread adoption in the biological sciences of open source
software, the R programming language in particular \citep{R2013}, has
led to a surge of field-specific packages
\citep[code libraries; e.g.][]{Metcalf2013, Dowle2013, Colchero2012} and
instructional literature
\citep[e.g.][]{Clark2007, Zuur2009, Crawley2012, Bolker2008}. This is
leading to a reverse “tower of babel” situation, where ecologists are
moving away from a diverse suite of programming languages towards
speaking a common computational tongue. Hence, we feel that it makes
sense to focus on techniques that are directly applicable to R and we
will make use some realistic program examples from biology to highlight
effective strategies to speed-up code.

We give guides on profiling, general inefficiencies in R, parallel
computing and extending R with C. All code examples supplied here should
work on all platforms, unless indicated separately. We assume that you
have a working R installation (R $>$ 3.0.0), and have modified little to
nothing about the installation yourself [^2]. All code examples and
benchmarks in this document were run on a single test machine (MSI model
gt680R, with CPU Intel i7-2639QM, 8GB DDRII RAM running Fedora 17), code
was also tested separately on other systems (Windows and Mac).

General guidelines for optimization
-----------------------------------

[generalguidelines]

Learning how to code neatly and efficiently is key to solving complex
computational problems quickly and in the long-run we feel that everyone
will be better off learning the fundamentals of efficient coding
\citep[see e.g. programming guides for scientists][]{Wilson2012}. In
general, however, the following steps are advocated by computer
scientists when attempting to optimize code
\citep[see e.g.][]{Wilson2012, Chambers2009}. They are meant to ensure
that you are working correctly and productively. We will follow these
steps throughout this document.

1.  Your first objective should always be to make sure that your program
    does what you intended it to do correctly. Start by using easy to
    debug R code, without worrying too much about efficiency initially.
    Once you have a working program with results you can trust, you have
    a solid basis to compare more complex optimized code against.

2.  If you believe performance should and can be improved, start
    profiling your program (as shown in the section [profiling]) or a
    smaller version of your program. This will help you to discover what
    is using most of the resources. Find out whether it is worthwhile to
    spend time optimizing using Amdahl’s law (section [amdahl] to
    calculate your expected gains (we use our R package $aprof$ for
    this).

3.  Once you find bottlenecks, and are certain that optimizing your code
    is worthwhile, you should start by asking yourself if you can speed
    up your R code itself. For example, is there something that you can
    vectorise? Can you drop a higher level function with more overhead
    for a lower level one (e.g. lm vs lm.fit)? Can you replace a slow
    function with a custom one?

4.  If you are still not satisfied with the program’s performance, then
    ask yourself if your code can run in parallel (section [parallel])?
    Or you may be able to rewrite certain key parts in C, C++ or Fortran
    (section [callingC]).

5.  At each step, repeatedly confirm for yourself that each version of
    code is giving the same answers as the correct, but slower, R code.

What slows down computation?
============================

[whatslowsdown] In general, there are two types of programming
languages: interpreted (e.g., R, Matlab) and compiled (e.g. C, FORTRAN).
Understanding the differences between these types provides insight into
some sources of computational inefficiency and their solutions. In
interpreted languages, like R, code is indirectly evaluated by an
evaluation program \citep[hereafter the R-interpreter][]{Chambers2009}.
In compiled languages, like C, code is first translated to machine
language (i.e. machine-specific instructions) by a compiler program and
then directly executed on the central processing unit (CPU). These
differences in the type of programming language used can have large
effects on execution speed \citep{Chambers2009}. Compiled and
interpreted languages exhibit a trade off in machine-run time versus
programmer time, respectively. Interpreted languages have the benefits
of being relatively easy to understand, debug and alter. However, there
is usually much higher CPU overhead as each line must be translated
(i.e. “interpreted”) every time it is executed. Compiled languages tend
to be more challenging to code and debug but are highly efficient when
executed as “translation overhead” occurs once when the source code is
compiled. Removing this overhead can result in considerable speed-ups
(Fig. [fig:slowdown]).

[!ht]

![image](figure/Interpreterquirksfigure)

[fig:slowdown]

A classic example of translation overhead can be seen in Figure 2 (a-h),
which shows the execution time of two sets of four identical functions
that simply evaluate the quantity N/N1 (code in section [profiling]),
only differing in the number of parentheses [^3] and whether the
language was compiled or interpreted. We immediately see that the
examples using compiled code in C are approximately 9 times faster, on
average. The reason for this is simple: the function has already been
processed by the compiler into a readily executable form, while for the
pure R code, some processing is required even at run time to interpret
the instructions at every iteration. Additionally, we can see that
simply adding parentheses, which causes extra work for the interpreter,
increases computation time in R (Fig. [fig:slowdown] a, c, e & g). The
slow-down is non-trivial, with an average increase in execution time of
14.7% per added pair of parentheses on our test machine. Compiled
languages, in contrast, are immune to this overhead (Fig. [fig:slowdown]
b, d, f & h). A major benefit of statistical software like R, however,
is that it employs a wide variety of statistical tools and programs that
work “out of the box”. These predefined programs often contain
additional features meant to ease their use by auto-formatting data,
error checking, and making other checks on user inputs. When dealing
with computationally onerous problems these features can cause
considerable slowdowns. For example, in Fig. [fig:slowdown] (i & j) we
see that using R’s ‘mean()’ function is less efficient than a more basic
function with the same result: (sum(x) / length(x)). The additional
features in ‘mean’ function make it slower but more robust and easier to
use. This generality is lost in the more basic function.

Common sources of inefficiency in R
-----------------------------------

Code optimization should start with making sure the code performs its
intended task as efficiently as possible by eliminating non-essential
procedures. This includes eliminating unnecessary function calls,
printing statements, plotting, or memory references (e.g. transposing
the same matrix or calculating the same value repeatedly within a loop).
Loops in code exacerbate inefficient code contained within the loop, and
can easily prove to be one of the most basic processing bottlenecks. A
valuable feature of R is vectorisation, which can be used to avoid the
inherent inefficiency of looping. Vectorisation involves writing
functions that are designed to work on vectors of values in one call.
Researchers switching from a different language may at first construct
loops that apply a function over a vector (e.g., when adding a series of
numbers [a vector] to a single number, one could loop through the series
adding each in turn). For instance, in ecological analyses it is often
necessary to create a matrix of random numbers (e.g., null model tests
for species co-occurrence). If we were to fill in random numbers in a
large matrix using a double loop (example 2.1), this would be
approximately 38 times slower than a vectorised approach (example 2.2
creating random sequences for entire rows). Avoiding loops all together
in R, by directly filling the appropriate sized matrix (example 2.3) is
approximately 63 times faster than the double loop (2.1).

[code example1.1]

[codeexample1.2]

[codeexample1.3]

The vectorised version is faster because creation of a long vector of
random numbers though a loop is already implemented in a lower-level
language \citep[in most cases C;][]{Schmidberger2009}. Vectorisation in
R is essentially a shift to a lower-level function when the user applies
a vectorised function (e.g. functions like runif, sum and many others).
This is far more efficient than using the R interpreter to loop through
each instance, and calculations conducted in a vectorised fashion are
thus often an order of magnitude faster (example 2.1. v.s. 2.2 v.s.
2.3). Some useful and highly optimized vectorised functions include:
*cumsum, diff, rowSums, colSums, rowMeans* and *colMeans*. Note that
example 2.5 can also be replaced with a single line of code using the
“apply” family of functions; “mat = apply(mat,1, function(X)
runif(length(X)))”. In this instance, however, apply is not necessarily
faster than a vectorised for loop (it is a myth is that the apply family
of functions are always faster than for loops). An easy alternative to
loops for beginning programmers in R is the function Vectorize which is
able to vectorise almost any function in R. Another inefficient coding
practice is “growing data”, where data frames, matrices, or vectors are
created without defining their size a priori, and values (e.g.
population states) are added to them incrementally in a loop.

[codeexample2.4]

When growing data, instances of memory allocation and run time are
significantly increased. In each iteration, the current amount of space
dedicated to vec is too small to store the new version of vec (i.e.
c(vec, runif(1)) ). Hence, an entirely new object must be written to
free space in the memory for each item added. In the next iteration the
space will be too small again and the process repeats itself becoming
ever more time consuming. A better practice is to pre-allocate space in
the memory with placeholders. For instance:

[codeexample2.5]

Here the code numeric(n) creates a vector of the correct size n in the
memory before values are assigned. Using memory profiling, we found that
example 2.4 performs twice as many memory allocations (1890) as example
2.2 (820), with the latter being 8 times faster on average (when n =
1000). Another potential speed-up strategy is to create custom functions
to avoid overhead in base- or package-provided functions. Custom
functions can be specialized to perform only the desired task, and can
lead to significant speed-ups. In Figure 1 (i & j) we saw that replacing
the “base” function mean with a custom function that sums a vector and
divides by its length (sum(x)/length(x)) can increase computational
speed six-fold. In our bootstrap example below (section
[bootstrappartone]), we provide another case where writing a custom
function provides improvement. Another strategy would be to use
lower-level functions instead of their default counterparts (e.g. lm.fit
vs lm). Note that these should be used cautiously as they require much
stricter compliance to input rules, being not as robust against poor
user inputs as the default high-level functions.

When is it worthwile to optimize?
=================================

[amdahl]

One should consider optimization only after the code works correctly and
produces trustworthy results \citep{Chambers2009}. One should also
consider whether cruder code would in the end be more time-efficient,
and so it is important to recall a fact that is recognized by
programmers: *"everyone knows that debugging is twice as hard as writing
a program in the first place. So if you’re as clever as you can be when
you write it, how will you ever debug it?"* \citep{Kernighan1978}.

Often, only a portion of the code can be optimized. Applying Amdahl’s
law \citep{Amdahl1967} can help make the decision of whether to pursue
the optimization. Consider a computation that requires time $T_0$ to
complete and that we are interested in optimizing a portion of that
code. If the portion to optimize requires a fraction $\alpha$ of the
total time ($T_0$) to complete, and can be improved by a factor I we can
calculate the overall improved execution time ($T_i$) of the entire
computation, as:

$$T_i=(1-\alpha) T_0+(\alpha T_0)/I$$

The first term on the right hand side of (1) describes the amount of
time required to process the portion of the code that was not optimized.
The second term describes the amount of time to process the optimized
portion. It follows that the realized speedup factor (S) can be
expressed as:

$$S=1 / (( 1 - \alpha )+ \alpha /t)$$

Amdahl’s law reveals that the effect of optimization on the overall
program performance will depend on both the improvement caused by the
optimization (I), and the fraction of the program that can be improved
($\alpha$) (Fig. [fig:amdahl]). For example, suppose a specific
operation within R’s program code originally took 50% of the execution
time ($\alpha$ = 0.5), and this operation can be rewritten in a compiled
language causing an expected factor 9 speed up (I = 9). Despite the
substantial improvement in the operation, the overall improvement is far
less (S = 1.8 times faster). In computer science this is seen as the
major insight of Amdahl’s Law \citep{Bryant2010}: unless one improves a
very large fraction of the overall system, the realized improvement in
execution time will be considerably less.

[!ht]

![image](figure/Amdahlslaw)

[fig:amdahl]

Profiling
---------

[profiling] Empirical studies in computer science show that inefficiency
is often rooted in a small proportion of code \citep{Porter1990}.
Therefore, identifying which part of the code takes the most time can
allow effective and targeted optimization efforts. “Code profilers” help
towards this end; these are software engineering tools that measure the
performance of different parts of a program as it executes
\citep{Bryant2010}. In this section we will describe how to use a
statistical profiler (called $Rprof$ in R), that uses “operating system
interrupts” to probe which routines are active at regular intervals and
counts which R expressions are consuming the most resources.

In cases where random-access memory (RAM) storage, rather than processor
time, poses the most pressing limit, one can use a memory profiler (e.g.
Rprofmem) to obtain similar statistics for memory efficiency. Simpler,
though less informative, tools to time and track resource use include
the R functions system.time() and object.size(). The former returns the
time used by both R and the operating system (for communication between
devices, file writing, etc.), while the latter gives an approximation of
the memory usage of objects.

Throughout the tutorial we will make heavy use of R’s profiling
capabilities. R uses a so-called statistical profiler (Rprof), which
probes at predefined intervals which routines are active and so we can
count which R expressions are consuming most of the system’s resources.
For example, profiling a program consisting of a two commands will tell
you exactly how much time is spent in each command, and therefore which
of the two you should take a closer look at. We have also developed
software in the form of an R package (aprof: “Amdahl’s profiler”) to
organize the output from the R profiler using visual tools to identify
bottlenecks (as illustrated in Figure [NaiveFigure]).

In this document we will use the package aprof to organise the output
from R’s standard profiler and makes it much easier to rapidly identify
bottlenecks in your program code. An example of using $aprof$ is given
below. The package is available at CRAN.

To use this profiler, we first need to make a simple program which we
want to profile. Here is an example of a program (InterpreterQuirks)
that executes the calculation $N/(1+N)$ many times with either
parentheses or brackets, and different amounts of each. The function
highlights some of the quirks of an interpreted language as explained
earlier (section [whatslowsdown]).

[MakeInterpreterQuirks]

Next we use Rprof to start profiling $InterpreterQuirks$. We will first
reload our saved file so the code lines in the R environment, as this
ensure that the version in R and the saved file on the disk match up
exactly.

[ReloadInterpreterQuirks]

Then we switch on R’s profiler $Rprof$, and because we want to know
which lines of InterpreterQuirks are the slowest we need to make sure
the option $line.profiling$ is set to TRUE. Here we use a time interval
between samples of 0.02.

[ProfileInterpreterQuirks]

Next we load our package $aprof$ ("Amdahl’s profiler"). We want to
visualize the time spent in each line of code using $aprof$’s standard
plot function on our program $InterpreterQuirks$ (see $?plot.aprof$ for
details). However before we start we need to use the function $aprof$ to
make an *"aprof* object". This object will contain the profiling
information and the source file $"InterpreterQuirks.R"$.

[makeaprofclass]

**

Now that we have a standard $"aprof object"$ we can display some basic
information about the profiling exercise by simply typing the name of
the $"aprof object"$ and hitting return.

[aprofprint]

    ## 
    ## Source file:
    ## InterpreterQuirks.R (7 lines).
    ## 
    ##  Call Density and Execution time per line number:
    ## 
    ##       Line  Call Density  Time Density (s)
    ## [1,]  3     4             0.08            
    ## [2,]  5     4             0.08            
    ## [3,]  6     5             0.1             
    ## [4,]  4     6             0.12            
    ## 
    ##  Totals:
    ##  Calls        20 
    ##  Time (s)     0.46   (interval =      0.02 (s))

Next we can use the standard $plot$ function on this object to display
the execution time per line. The following code should return figure
[fig:intquirks].

![image](figure/FigureInterpreterQuirks)

[fig:intquirks]

Plotting an *aprof* object is useful when your program is relatively
small however, when your code consists of hundreds of lines, a better
function would be $profile.plot$. Go ahead and use it on our *aprof*
object "IntQuirksAprof" to see what it does (type $?profile.plot$ in the
command line for details). Another useful feature is to summarize our
$aprof$ object, which gives us the theoretical maximum attainable
speed-up for each line of code (see $?summary.aprof$ for details).

[InterpreterQuirks]

    ## Largest attainable speed-up factor for the entire program
    ## 
    ##         when 1 line is sped-up with factor (S): 
    ## 
    ##   Speed up factor (S) of a line 
    ##             1     2     4     8     16    S -> Inf**
    ## Line*: 4 :  1.00  1.15  1.24  1.30  1.32  1.35      
    ## Line*: 6 :  1.00  1.12  1.19  1.23  1.26  1.28      
    ## Line*: 3 :  1.00  1.10  1.15  1.18  1.19  1.21      
    ## Line*: 5 :  1.00  1.10  1.15  1.18  1.19  1.21      
    ## 
    ## Lowest attainable execution time for the entire program when
    ## 
    ##              lines are sped-up with factor (S):
    ## 
    ##   Speed up factor (S) of a line  
    ##             1       2       4       8       16    
    ## All lines   0.4600  0.2300  0.1150  0.0575  0.0288
    ## Line*: 4 :  0.4600  0.4000  0.3700  0.3550  0.3475
    ## Line*: 6 :  0.4600  0.4100  0.3850  0.3725  0.3663
    ## Line*: 3 :  0.4600  0.4200  0.4000  0.3900  0.3850
    ## Line*: 5 :  0.4600  0.4200  0.4000  0.3900  0.3850
    ## 
    ##     Total sampling time:  0.46  seconds
    ##  *  Expected improvement at current scaling
    ##  ** Asymtotic max. improvement at current scaling

Using this information we can easily decide where to focus our efforts
and, maybe more importantly, decide whether it is worth the effort to
optimize the code. As we can see in the uppermost table, line 4 would be
the most promising to work on, as it shows the greatest improvement for
each of the sets of speed-up factors (1 - 16$\times$). These numbers
effectively tell us what the predicted overall speed-up of the program
would be when we focus on a single line. That is, if we improve the
execution time of a given line by a factor S (S times faster), the table
predicts how much this improvement will affect the overall run-time of
the entire program. In the above example we see, however, that the gain
is minimal and even when the speed-up factor goes to infinity
(effectively when the run time of that line becomes 0;
$\lim S \to \infty$) we can only achieve a maximum speed-up of 1.39. Or
in other words, if we were to infinitely improve the code in line number
4 we would only improve the overall program by 39%. Infinitely faster is
not something we are likely to achieve, but also for the more practical
speed-up factors we also see that a factor of 16 improvement is hardly
an improvement over a factor of 4. In such cases it may not be
worthwhile to either purchase computing resources (parallel machines or
execution time on a cluster) or spend time optimizing code. Naturally,
in this simple example the overall execution time is so small that we
won’t spend time optimizing it. However, as we usually would profile a
simplified version of a larger program where the execution time may be
considerably larger, even a 36% improvement in execution time may be
worthwhile.

Optimizing R code: a bootstrap example
======================================

[parallel] Now that we are a little bit familiar with profiling, let’s
continue with a more practical example. In the next sections we will be
inspecting some methods with which we can speed-up a bootstrap
operation. We start with a simple bootstrap program that is a typical
example of "unpolished" code, and then continue to profile and optimize
the code in a series of steps. In the final sections, we run our highly
optimized code in parallel on multiple processors to obtain a 17.5
$\times$ speed-up (see the main document for more details).

Bootstrapping a moderately large biodiversity dataset
-----------------------------------------------------

[bootstrappartone] Imagine we have a moderately large biodiversity
dataset, with 750 000 records, which may resemble something like species
counts from $N$ plots at $S$ different sites
\citep[e.g.][]{Hennekens2001}. To create such a fake dataset in R, we
could use the following code:

Our goal is to calculate for each site 10 000 bootstrapped means of
species richness. Using $base$ R, however we have a problem, for
although our dataset is not particularly large, the bootstrap routine in
R (the function $boot$, in the $boot$ package) can’t handle its size.
After first defining how many bootstrap resamples (10 000) we want:

[booterror]

We get the following error[^4].

    Error in sample.int(n, n * R, replace = TRUE) : invalid 'size' argument
    In addition: Warning message:
    In sample.int(n, n * R, replace = TRUE) : NAs introduced by coercion

This error occurs because within the boot function all re-samples are
generated in one go. This ensures that random numbers will be
independent even when $boot$ is run in parallel[^5], but causes problems
when datasets become moderately large (as we reach the limits of the
internal R function
$sample.int(7.5 * 10^5, 7.5*10^5 * 10000, replace = TRUE)$ which is used
"behind the scenes" in $boot$).

A way around this problem would be to define some function that will
execute a standard amount, say 10 000 re-samples of the dataset and
calculate our statistics of interest. The following code will conduct a
bootstrap as required by our goal, albeit not quite optimally. Let’s
call this function $NaiveBoot$ and save it as a file called
$NaiveBoot.R$.

[DefineNaiveboot]

Before we run the full bootstrap operation, let’s see how fast our
program runs by profiling it using a smaller problem. In essence a
bootstrap problem has two aspects that will influence run time: 1) the
size of the dataset that needs to be reshuffled and 2) the amount of
resamples. So let’s reduce the problem by conducting only 10% of the 10
000 resamples on only 10% of the dataset. We can now be confident that
we have decreased the size of our problem considerably.

[subsetingbiodata]

Next we start the profiling operation (again, we should not forget to
switch on line-profiling in the *Rprof* function call).

[ProfileNaiveboot]

Now that we have the R profiler output, we can find out where the
bottlenecks are in our program.

![image](figure/AprofNaiveboot)

[fig:naiveboot] [NaiveFigure]

In this case it seems like there can be large gain by looking carefully
at line 6 (see Fig. [NaiveFigure]). First, however, we should evaluate
whether its worthwhile to attempt to optimize the code.

[NaiveBootrun]

    ## Largest attainable speed-up factor for the entire program
    ## 
    ##         when 1 line is sped-up with factor (S): 
    ## 
    ##   Speed up factor (S) of a line 
    ##             1      2      4      8      16     S -> Inf**
    ## Line*: 6 :   1.00   1.87   3.30   5.34   7.74  14.06     
    ## Line*: 5 :   1.00   1.04   1.06   1.06   1.07   1.07     
    ## 
    ## Lowest attainable execution time for the entire program when
    ## 
    ##              lines are sped-up with factor (S):
    ## 
    ##   Speed up factor (S) of a line  
    ##             1      2      4      8      16   
    ## All lines   27.28  13.64   6.82   3.41   1.71
    ## Line*: 6 :  27.28  14.61   8.28   5.11   3.52
    ## Line*: 5 :  27.28  26.33  25.86  25.62  25.50
    ## 
    ##     Total sampling time:  27.28  seconds
    ##  *  Expected improvement at current scaling
    ##  ** Asymtotic max. improvement at current scaling

The output from the function $summary$ basically confirms what we saw
when eye-balling line 6 in figure [NaiveFigure], but we now know that
worthwhile speed-ups are at least theoretically possible [^6]. We will
run through the table output again quickly. We see that optimizing line
6 will give the greatest returns for our efforts (which can be expressed
by the speed-up factor "S"). For instance if we figure out a way to make
line 6 execute 4 times faster the overall execution time of the program
will speed-up by a factor of more than 3.6 (upper table) while the time
of the full program’s completion is projected to drop to almost 11
seconds (bottom table, at S = 4). Therefore, we should clearly focus on
line 6 at this stage. As discussed in the main document, we can identify
the usual suspects slowing down our operations: line 6 contains a
reference to *rbind*, and here we are growing an object to store our
results. This is highly inefficient. Also, we have shown that some
standard R-functions like the default $mean$ have overhead that can slow
down computations (see main document for more details). Our next step
could therefore be to pre-allocate space in the memory for our results,
and use a custom mean function. Thus, we pre-allocate memory for our
bootstrapped means:

[Preallocate]

And we replace the $mean$ function with a custom $avg$ function. Here we
get rid of much of the functionality in the ‘mean()’ function that makes
it work easily with different data structures and provides various
checks to the data and arguments.

[DefineAvg]

Instead of creating a custom function, you could switch to a
"lower-level" version of a function. For instance, if the function $lm$
is taking most of the time in your calculations, then you can use its
lower-level function $lm.fit$. This lower-level function is much faster.
However, it less robust to bad user input, so watch out with your
inputs! You can often find such lower-level functions by looking at the
source code of a "slow" function (using e.g. $page("lm")$ is one way of
doing this). A general guide to find the source code of a function (and
any lower-level functions) is given in \citep{Ligges2006}.

Our new bootstrap function $LessNaiveBoot$ then looks like this;

[DefineLessNaiveboot]

Next, let’s see what we have acheived by timing the run of our updated
code.

We see that pre-allocation has given us a clear improvement! Starting
off with an execution time of about 45 seconds, we were able to shave
off 15 seconds from the execution time (an improvement of roughly 33%)
by removing some obvious causes of slowdown in our code. However, were
they the best candidates for a speed-up? Or, is there more room for
improvement? Let’s take a detailed look at our improved function to find
out.

[ProfileLessNaiveboot]

[AprofLessNaiveBootResults]

    ## Largest attainable speed-up factor for the entire program
    ## 
    ##         when 1 line is sped-up with factor (S): 
    ## 
    ##   Speed up factor (S) of a line 
    ##             1     2     4     8     16    S -> Inf**
    ## Line*: 7 :  1.00  1.75  2.80  4.00  5.09  7.00      
    ## Line*: 5 :  1.00  1.06  1.09  1.10  1.11  1.12      
    ## 
    ## Lowest attainable execution time for the entire program when
    ## 
    ##              lines are sped-up with factor (S):
    ## 
    ##   Speed up factor (S) of a line  
    ##             1     2     4     8     16  
    ## All lines   1.12  0.56  0.28  0.14  0.07
    ## Line*: 7 :  1.12  0.64  0.40  0.28  0.22
    ## Line*: 5 :  1.12  1.06  1.03  1.01  1.01
    ## 
    ##     Total sampling time:  1.12  seconds
    ##  *  Expected improvement at current scaling
    ##  ** Asymtotic max. improvement at current scaling

Eye-balling figure [LessNaiveFig], we see that most time is spent in
line 7, and the table returned by $aprof$ above shows that it should be
worthwhile to further optimize the functions in line 7. However, we now
also see that it is no longer quite as obvious where we should focus our
optimisation efforts in line 7, because there are several functions
active in that line (see figure [LessNaiveFig]).

[!ht]

![image](figure/FigureLessNaiveBoot)

[LessNaiveFig]

To help identify which function is slowing down our calculations in line
7, we will next take a targeted look at the functions in line 7. To
achieve this we can use $aprof's$ function $targetedSummary$ and take a
more detailed look at the functions being called in line 6. We now
clearly see which functions are taking the most time within line 6 of
our code. The function $targetedSummary$ returns the execution time
spent within each function, expressed both in the amount of calls in the
record returned by the R profiler and in run time. Please, note that
when a function within another function is ’called’ by the R profiler,
both functions get a hit (’Calls’). In this case, therefore, the
outermost function $tapply$ gets the most ’Calls’. Let’s run
$targetedSummary$ and print the first ten elements, which are the ten
most-frequently called functions:

[TargetLessNaiveboot]

    ##         Function Calls Time
    ## 1         tapply    48 0.96
    ## 2         lapply    31 0.62
    ## 3          split    22 0.44
    ## 4  split.default    22 0.44
    ## 5      as.factor    11 0.22
    ## 6    clipboard#2     8 0.16
    ## 7            FUN     7 0.14
    ## 8         unlist     6 0.12
    ## 9          match     5 0.10
    ## 10          sort     5 0.10

We see that the vast majority of time is spent within such R functions
as $tapply$, $lapply$, $split$, $split.default$, $as.factor$ and
$factor$. However, as we can see in figure [LessNaiveFig], we never used
the functions $lapply$, $split.default$, $as.factor$ or $factor$ in line
7 . Upon investigation we see that these functions show up in the output
from $targetedSummary$ because they are are called within $tapply$ (type
$page(tapply)$ to use R’s pager to confirm this in the code of tapply).
The function $tapply$ is their parent function. We can get this
information directly from $targetedSummary$ by setting the "findParent"
option to "TRUE":

[FindParentTargetLessNaiveboot]

    ##         Function        Parent Calls Time
    ## 1         tapply            L7    48 0.96
    ## 2         lapply        tapply    31 0.62
    ## 3          split        lapply    22 0.44
    ## 4  split.default         split    22 0.44
    ## 5      as.factor split.default    11 0.22
    ## 6    clipboard#2           FUN     8 0.16
    ## 7            FUN        lapply     7 0.14
    ## 8         unlist        tapply     6 0.12
    ## 9          match     as.factor     5 0.10
    ## 10          sort     as.factor     5 0.10

Now we see the nesting of all functions within their parent functions,
we see that $tapply$ is called in line 7 (L7), $lapply$ within $tapply$,
$split$ within $lapply$ and so on. We now can conclude that most time is
spent in the function $tapply$ which calls $lapply$. Within $lapply$ the
most time is spent executing the function $split$, $split.default$,
$as.factor$ and $factor$ (and nearly no time is spent below $factor$).
If we investigate what these functions do we will see that they are
concerned with ordering our data into groups, from which we calculate
site means. Clearly, it is the grouping of our $data.frame$ containing
our pseudo-ecological dataset that is causing the greatest slowdowns.
This was likely also slow in our previous function (you can try using
$targetedSummary$ on the output from $NaiveBoot$ to confirm this).

R "data.frames" are in essence special forms of lists, that are very
useful as they store multiple types of data (e.g. integers, characters,
factors) within one single $data.frame$. This usefulness seems to come
at a cost as they are also notoriously slow. To speed-up execution time
here, there are a few options left for us at this point. We could
abandon the $data.frame$ class and adopt a matrix form, where either
each column or row contains the data for a site, we can then calculate
means with the highly optimized functions $colMeans$ or $rowMeans$. This
would work in this simple example, but not all datasets will be easily
converted to matrices (a matrix can only contain one type of data).
Alternatively, we could move the handling of data outside of R, calling
a language more suited for large datasets (e.g. SQL, perl). Ultimately,
we could also do both while staying within R by switching to the
$data.table$ class. This is an object class introduced by
\citet{Dowle2013} in their package $data.table$. The $data.table$
package is a useful package for fast indexing and grouping of data[^7].
We will use the $data.table$ package here as an easy way to speed-up
indexing and grouping of large datasets in R.

First we need to store our dataset as a $data.table$, which is easily
done with the following lines of code (you might need to install the
$data.table$ package first):

[Datatable]

Next we will redefine our bootstrap function using $data.table$ and test
our improvement (for more details on capabilities see $data.table$’s
documentation by using $vignette("datatable-intro")$ when you have
installed the package).

[DefineDTboot]

The most significant change is in the line of code that replace the
original $tapply$ statement:
*results[i,]$<$-x[index,mean(S),by=site]\$V*. Timing the new function
gives us:

    system.time(Results<-DatatableBoot(subBioDatatable,subR))
      user  system elapsed 
     12.692   0.004  12.718 

In this case, by targeting the slowest part of the code we have created
a 3.78 speed-up factor compared to our original "naive code". Now that
we are confident we have a sufficiently efficient program in serial,
let’s start our final speed-up exercise; running our code in
parallel[^8].

