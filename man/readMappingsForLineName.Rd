% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathMatrix.R
\name{readMappingsForLineName}
\alias{readMappingsForLineName}
\title{Retrieve read mapping information from PHG database.}
\usage{
readMappingsForLineName(
  configFile,
  lineName,
  readMappingMethodName,
  haplotypeMethodName,
  fileGroup = NULL
)
}
\arguments{
\item{configFile}{Path to a configuration file for your graph database.}

\item{lineName}{The name of the line (taxon) for which the read mapping
information is to be retrieved. If there are multiple read mappings with
different \code{file_group_names}, they will be combined.}

\item{readMappingMethodName}{The method name for the read mappings
(only takes a single method).}

\item{haplotypeMethodName}{The haplotype method name.}

\item{fileGroup}{the name of the file group for the line from the database.
This parameter is only necessary if the line (taxon) has more than one
file group and only the reads for a specific file group are wanted.}
}
\description{
Returns an \code{S4Vectors} \code{DataFrame} object of read
  mapping information for a given line (i.e. taxon).
}
\author{
Brandon Monier

Peter Bradbury
}
