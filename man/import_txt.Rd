% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-txt.R
\name{import_txt}
\alias{import_txt}
\title{Import text file}
\usage{
import_txt(filepath)
}
\arguments{
\item{filepath}{a string indicating the relative or absolute
filepath of the file to import}
}
\value{
a [tibble][tibble::tibble-package] of each row
  corrresponding to a line of the text file, with the column named
  "text"
}
\description{
Import text file
}
