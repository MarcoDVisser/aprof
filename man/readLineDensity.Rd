\name{readLineDensity}
\alias{readLineDensity}
\title{readLineDensity}
\usage{
  readLineDensity(aprofobject = NULL, Memprof = FALSE)
}
\arguments{
  \item{aprofobject}{An object an object returned by
  \code{aprof}, which contains the stack calls sampled by
  the R profiler.}

  \item{Memprof}{Logical. Should the function return
  information specific to memory profiling? As memory use
  per line and Mb counts? Otherwise the default is to
  return line call density and execution time counts. This
  feature is unavailable in the current version.}
}
\description{
  Reads and calculates the line density (in execution time
  or memory) of an aprof object returned by the
  \code{aprof} function. If a sourcefile was not specified
  in the aprof object, then the first file within the
  profiling information is assumed to be the source.
}
\author{
  Marco D. Visser
}

