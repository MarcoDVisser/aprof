\name{plot.aprof}
\alias{plot.aprof}
\title{plot.aprof}
\usage{
  \method{plot}{aprof} (x, y, ...)
}
\arguments{
  \item{x}{An aprof object as returned by apof(). If this
  object contains both memory and time profiling
  information both will be plotted (as proportions of total
  time and total memory allocations.}

  \item{y}{Unused at current.}

  \item{\dots}{Additional printing arguments. Unused at
  current.}
}
\description{
  Plot execution time per line of code from a previously
  profiled source file. The plot visually shows bottlenecks
  in a program's execution time, shown directly next to the
  code of the source file.
}
\examples{
\dontrun{
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
}
\author{
  Marco D. Visser
}
\concept{
  Line profiling
}

