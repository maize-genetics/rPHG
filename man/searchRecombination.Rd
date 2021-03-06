% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statsAndVisualization.R
\name{searchRecombination}
\alias{searchRecombination}
\title{Search for recombination}
\usage{
searchRecombination(
  phgObject = NULL,
  gameteName,
  refRangeSame,
  refRangeDiff,
  phgHapIDMat = NULL
)
}
\arguments{
\item{phgObject}{A PHG object.}

\item{gameteName}{A specified gamete name}

\item{refRangeSame}{See description for further details.}

\item{refRangeDiff}{See description for further details.}

\item{phgHapIDMat}{The output of the \code{hapIDMatrix()} function. If
\code{NULL}, A hap ID matrix will be generated (if you have supplied a
PHG object).}
}
\description{
Search for inbred lines (gametes) that are the same in one
  range but different in another. Such lines have experienced recombination
  in the past relative to each other. Must specify a gamete name and
  reference ranges.
}
