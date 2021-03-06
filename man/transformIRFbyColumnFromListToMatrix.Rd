% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformIRFbyColumnfromListToMatrix.R
\name{transformIRFbyColumnFromListToMatrix}
\alias{transformIRFbyColumnFromListToMatrix}
\title{transformIRFbyColumnFromListToMatrix}
\usage{
transformIRFbyColumnFromListToMatrix(listX, labelShock, n)
}
\arguments{
\item{listX}{list of square n x n matrices of dimension h}

\item{labelShock}{a numeric scalar, a column index to extract.}

\item{n}{a numeric scalar,}
}
\value{
matrix h x n, each jth column of a hth list element corresponds to the hth row
}
\description{
Auxiliary function that extracts responses of the variables to one shock
}
