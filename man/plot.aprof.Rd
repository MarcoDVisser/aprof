\name{plot.aprof}
\alias{plot.aprof}
\title{plot.aprof}
\usage{
  plot.aprof(aprofobject, zoom = NULL)
}
\arguments{
  \item{aprofobject}{An aprof object as returned by apof().
  If this object contains both memory and time profiling
  information both will be plotted (as proportions of total
  time and total memory allocations.}

  \item{zoom}{Zoom into a particular section of code. Can
  either be set to "TRUE" or line numbers can be given. If
  line numbers are given as c(min,max), then the function
  will attempt to zoom in between these, otherwise "TRUE"
  will result in a zoom centred around the lines with the
  greatest density in execution time (and/or memory).}
}
\description{
  Plot execution density per line of code from a previously
  profiled source file. The plot visually shows bottlenecks
  in program execution time, directly where most
  programmers are most familiar with their code: the source
  file.
}
\examples{
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
}
\author{
  Marco D. Visser
}

