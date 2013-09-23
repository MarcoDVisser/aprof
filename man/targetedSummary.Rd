\name{targetedSummary}
\alias{targetedSummary}
\title{targetedSummary}
\usage{
  targetedSummary(target = NULL, calls, interval = 0.02,
    sourcefile = NULL, findParent = FALSE)
}
\arguments{
  \item{target}{the specific line of code to take a
  detailed look at.}

  \item{calls}{Stack calls as returned by readOutput}

  \item{interval}{the profiler sampling interval}

  \item{sourcefile}{a plain text file (e.g. txt, .R)
  including the source code of the previously profiled
  program. If empty the function will attempt to extract
  this information from the stack calls.}

  \item{findParent}{Logical, should an attempt be made to
  find the parent of a function call? E.g. lm would be a
  parent call of lm.fit or mean a parent call of
  mean.default. Note that the option only returns the most
  frequently accociated parent call when multiple unique
  parents exist.}
}
\description{
  Allows a detailed look into certain lines of code, which
  have previously been identified as bottlenecks by
  PlotExcDens or aprof in combination with a source file.
}
\author{
  Marco D. Visser
}

