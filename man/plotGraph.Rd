% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_all_generics.R, R/class_phg_dataset.R
\name{plotGraph}
\alias{plotGraph}
\alias{plotGraph,PHGDataSet-method}
\title{Plot a graph object}
\usage{
plotGraph(object, ...)

\S4method{plotGraph}{PHGDataSet}(
  object,
  samples = NULL,
  sampleHighlight = NULL,
  seqnames = NULL,
  start = NULL,
  end = NULL,
  colMajor = "maroon",
  colMinor = "lightgrey"
)
}
\arguments{
\item{object}{A \code{PHGDataSet} object}

\item{...}{Additional parameters to pass for ref range inclusion}

\item{samples}{Samples/taxa to include in plot}

\item{sampleHighlight}{Sample path to highlight}

\item{seqnames}{A sequence (e.g. chromosome) ID}

\item{start}{Start position for ref ranges}

\item{end}{End position for ref ranges}

\item{colMajor}{Highlight path color}

\item{colMinor}{Muted path color}
}
\description{
Generates a visualization of a recapitulated graph object
}
