% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCompanionForm.R
\name{getCompanionForm}
\alias{getCompanionForm}
\title{getCompanionForm}
\usage{
getCompanionForm(mB)
}
\arguments{
\item{mB}{a n x np matrix of the AR coefficients of the reduced-form VAR
(without deterministic terms ). The coefficients are organised as follows:
Y1_{t-1}, Y2_{t-1}, Y1_{t-2}, Y2_{t-2}, ..., Y1_{t-p}, Y2_{t-p}.}
}
\value{
np x np companion form matrix
}
\description{
Constructs companion form from n x np matrix of RFVAR coefficients estimated by OLS.
}
