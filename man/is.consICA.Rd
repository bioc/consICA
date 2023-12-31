% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is.consICA}
\alias{is.consICA}
\title{Is the object is consensus ICA compliant?}
\usage{
is.consICA(cica)
}
\arguments{
\item{cica}{list}
}
\value{
TRUE or FALSE
}
\description{
Check if the object is a list in the same format as the result 
of `consICA()`
}
\examples{
# returns TRUE
is.consICA(list("ncomp" = 2, "nsples" = 2, "nfeatures" = 2, 
                "S" = matrix(0,2,2),"M" = matrix(0,2,2)))
}
