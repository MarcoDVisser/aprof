\name{summary.aprof}
\alias{summary.aprof}
\title{summary.aprof}
\usage{
  summary.aprof(aprofobject, type = "line")
}
\arguments{
  \item{calls}{Stack calls as returned by readOutput. Line
  profiling must be activated for this to work.}

  \item{interval}{the profiler sampling interval.}

  \item{type}{currently ignored. Only line profiling is
  available in this version.}
}
\description{
  Summarizes an "aprof" object and returns a table with the
  theoretical maximal improvent in execution time for the
  entire profiled program when a given line of code is
  sped-up by a factor (called S in the output).
  Calculations are done using using R's profiler output,
  and requires line profiling to be switched on. Expected
  improvements are estimated for the entire program using
  Amdahl's law (Amdahl 1967). Calculations are subject to
  the scaling of the problem at profiling. The table output
  aims to anwser whether it is worthwhile to spend hours of
  time optimizing bits of code (e.i. refactoring in C) and
  where these efforts should be focussed. Using aprof one
  can get estimates of the maximum possible gain for any
  optimization efforts. Such considerations are important
  when one wishes to balance development time vs execution
  time.
}
\author{
  Marco D. Visser
}
\references{
  Amdahl, Gene (1967). Validity of the Single Processor
  Approach to Achieving Large-Scale Computing Capabilities.
  AFIPS Conference Proceedings (30): 483-485.
}

