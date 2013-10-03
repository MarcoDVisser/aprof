\name{aprof}
\alias{aprof}
\title{Amdahl's profiler}
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
  Amdahl's profiler, directed optimization.
}
\details{
  Creates an "aprof" object from the R-profiler's output
  and a source file.

  The objects created by "aprof" can inturn be used by the
  standard functions plot, summmary and print (more
  specifically: \code{plot.aprof}, \code{summary.aprof} and
  \code{print.aprof}.
}
\author{
  Marco D. Visser
}
\seealso{
  \code{link{plot.aprof}}, \code{\link{summary.aprof}} and
  \code\{link{print.aprof}}.
}

