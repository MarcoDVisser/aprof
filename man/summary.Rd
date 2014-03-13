\name{summary.aprof}
\alias{summary.aprof}
\title{Projected optimization gains using Amdahl's law.}
\usage{
  \method{summary}{aprof} (object, ...)
}
\arguments{
  \item{object}{An object returned by the function
  \code{aprof}.}

  \item{\dots}{Additional unused arguments}
}
\description{
  summary.aprof, projections of code optimization gains.
}
\details{
  Summarizes an "aprof" object and returns a table with the
  theoretical maximal improvement in execution time for the
  entire profiled program when a given line of code is
  sped-up by a factor (called S in the output).
  Calculations are done using R's profiler output, and
  requires line profiling to be switched on. Expected
  improvements are estimated for the entire program using
  Amdahl's law (Amdahl 1967). Calculations are subject to
  the scaling of the problem at profiling. The table output
  aims to answer whether it is worthwhile to spend hours of
  time optimizing bits of code (e.g. refactoring in C) and
  where these efforts should be focussed. Using aprof one
  can get estimates of the maximum possible gain for any
  optimization efforts. Such considerations are important
  when one wishes to balance development time vs execution
  time. Predictions are subject to the scaling of the
  problem.
}
\author{
  Marco D. Visser
}
\references{
  Amdahl, Gene (1967). Validity of the Single Processor
  Approach to Achieving Large-Scale Computing Capabilities.
  AFIPS Conference Proceedings (30): 483-485.
}
\concept{
  Line profiling
}

