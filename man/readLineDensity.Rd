\name{readLineDensity}
\alias{readLineDensity}
\title{readLineDensity}
\usage{
  readLineDensity(calls, interval, TargetFile = NULL,
    Silent = FALSE)
}
\arguments{
  \item{calls}{Stack calls as returned by readOutput}

  \item{interval}{the profiler sampling interval}

  \item{TargetFile}{a plain text file (e.g. txt, .R)
  including the source code of the previously profiled
  program.}

  \item{Silent}{Logical. Should the function return summary
  information? Otherwise the default is to return line call
  density and execution time counts. Based on the profiler
  output organized by readOutput.}
}
\description{
  Reads and calculates the line density of an aprof object
  returned by the readOutput function. Returns summary
  information for the aprof object, when Silent = False.
}
\author{
  Marco D. Visser
}

