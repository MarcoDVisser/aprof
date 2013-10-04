\name{aprof}
\alias{aprof}
\title{Amdahl's profiler,  directed optimization.}
\usage{
  aprof(src = NULL, output = NULL, memoutput = NULL)
}
\arguments{
  \item{src}{The source code file name (and path if not in
  the working directory) of the program that as been
  profiled.}

  \item{output}{The file name (and path if not in the
  working directory) of a previously created profiling
  exercise.}

  \item{memprofile}{Optional. The file name (and path)}
}
\description{
  Create "aprof" objects for use with Amdahl's profiler
  functions.
}
\details{
  Creates an "aprof" object from the R-profiler's output
  and a source file. The objects created through "aprof"
  can be used by the standard functions plot, summmary and
  print (more specifically: \code{plot.aprof},
  \code{summary.aprof} and \code{print.aprof}). See the
  exampe below for more details.
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
     #display basic information, summarize and plot the object
     fooaprof
     summary(fooaprof)
     plot(fooaprof)
     profileplot(fooaprof)
}
\author{
  Marco D. Visser
}
\seealso{
  \code{\link{plot.aprof}}, \code{\link{summary.aprof}} and
  \code\{\link{print.aprof}}.
}

