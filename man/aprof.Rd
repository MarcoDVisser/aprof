\name{aprof}
\alias{aprof}
\title{Create an 'aprof' objects for usage in the package 'aprof'}
\usage{
  aprof(src = NULL, output = NULL, memoutput = NULL)
}
\arguments{
  \item{src}{The name (and path if not in the working
  directory) of the source code file. The source code file
  is a plain text file (e.g. txt, .R) containing the code
  of the previously profiled program. In some cases this
  may be left empty, as some functions in the "aprof"
  package will attempt to extract this information from the
  stack calls (the success of this will vary). Other
  functions require a defined source file: these will fail
  if the source file is not defined.}

  \item{output}{The file name (and path if not in the
  working directory) of a previously created profiling
  exercise.}

  \item{memoutput}{Optional. The file name (and path) of
  the memory profile from a previous profiling exercise.
  Not available in this version.}
}
\value{
  An aprof object
}
\description{
  Create an 'aprof' objects for usage with function in the
  package 'aprof'
}
\details{
  Creates an "aprof" object from the R-profiler's output
  and a source file. The objects created through "aprof"
  can be used by the standard functions plot, summary and
  print (more specifically: \code{plot.aprof},
  \code{summary.aprof} and \code{print.aprof}). See the
  exampe below for more details.
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
     #display basic information, summarize and plot the object
     fooaprof
     summary(fooaprof)
     plot(fooaprof)
     profileplot(fooaprof)
	}
}
\author{
  Marco D. Visser
}
\seealso{
  \code{\link{plot.aprof}}, \code{\link{summary.aprof}},
  \code{\link{print.aprof}}, \code{\link{Rprof}} and
  \code{\link{summaryRprof}}.
}
\concept{
  Line profiling
}

