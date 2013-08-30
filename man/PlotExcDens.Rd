\name{PlotExcDens}
\alias{PlotExcDens}
\title{PlotExcDens}
\usage{
  PlotExcDens(SourceFilename, outputfilename)
}
\arguments{
  \item{SourceFilename}{The file name (and path if not in
  the working directory) of source program.}

  \item{outputfilename}{The file name (and path if not in
  the working directory) of Rprof's output file.}
}
\description{
  Plot execution density per line of code from a given and
  previously profiled source file. It attempt to shows you
  visually where the bottlenecks in execution time are,
  right in the place where you where are most familiar with
  the code: your source file.
}
\author{
  Marco D. Visser
}

