% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index_guess.R
\name{index_guess}
\alias{index_guess}
\title{Helper function that guesses GoF or BoF from a given index name}
\usage{
index_guess(index)
}
\arguments{
\item{index}{A fit index or measure provided by function fitmeasures in package lavaan}
}
\value{
Returns GoF (Goodness-of-Fit index) or BoF (Badness of Fit index).
}
\description{
Helper function that guesses GoF or BoF from a given index name
}
\examples{
index_guess("cfi")
index_guess("tli")
index_guess("rmsea")
index_guess("srmr")
}
