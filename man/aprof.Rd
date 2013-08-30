\name{aprof}
\alias{aprof}
\title{aprof}
\usage{
  aprof(calls, interval, type = "line")
}
\arguments{
  \item{calls}{Stack calls as returned by readOutput}

  \item{interval}{the profiler sampling interval}
}
\description{
  aprof or "Amdahl's profiler", aims to make directed
  optimization easy by calculating the theoretical maximal
  speed up for each line of code (at current scaling) of
  the profiled program using Amdahl's law. It aims to help
  anwser question as, whether it is worthwhile to spend
  hours of time optimizing bits of code (e.i. refactoring
  in C)? What is going to be your maximum possible gain for
  the effort you put in? Or is it better to patient and
  just let the slow program calculate for days on a server
  somewhere? These considerations are important to know, as
  idealy one wishes to balance development time vs
  execution time... or we may end up gaining nothing. Aprof
  aims to help in this choice.
}
\author{
  Marco D. Visser
}
\references{
  Amdahl, Gene (1967). "Validity of the Single Processor
  Approach to Achieving Large-Scale Computing
  Capabilities". AFIPS Conference Proceedings (30):
  483-485.
}

