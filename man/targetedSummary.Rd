\name{targetedSummary}
\alias{targetedSummary}
\title{targetedSummary}
\usage{
  targetedSummary(target = NULL, calls, interval,
    sourcefile = NULL)
}
\arguments{
  \item{target}{the specific line of code to take a
  detailed look at.}

  \item{calls}{Stack calls as returned by readOutput}

  \item{interval}{the profiler sampling interval}

  \item{sourcefile}{a plain text file (e.g. txt, .R)
  including the source code of the previously profiled
  program.}
}
\description{
  Allows a detailed look into certain lines of code, which
  have previously been identified as bottlenecks by
  PlotExcDens or aprof in combination with a source file.
}
\author{
  Marco D. Visser
}

