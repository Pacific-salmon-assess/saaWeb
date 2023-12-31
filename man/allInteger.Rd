% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{allInteger}
\alias{allInteger}
\title{All Integers}
\usage{
allInteger(x, ignore_na = TRUE)
}
\arguments{
\item{x}{Values to check for integers}

\item{ignore_na}{Ignore NA values}
}
\value{
A single logical value based on all values being integers
}
\description{
Check that all values in a vector are integers
}
\examples{

saaWeb:::allInteger(letters) # FALSE
saaWeb:::allInteger(c("1", "2", "3", "4", "5", "6", "1000")) # TRUE
saaWeb:::allInteger("3.14") # FALSE

}
