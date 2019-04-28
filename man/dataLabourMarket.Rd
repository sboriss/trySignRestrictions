\name{dataLabourMarket}
\alias{dataLabourMarket}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
dataLabourMarket
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
The data are used in Baumeister, C. and J. D. Hamilton (2015)
Sign Restrictions, Structural Vector Autoregressions, and Useful Prior Information,
Econometrica 83(5), 1963-1999.
}
\usage{data("dataLabourMarket")}
\format{
  A data frame with 261 observations on the following 22 variables.
  \describe{
    \item{\code{w}}{a numeric vector}
    \item{\code{n}}{a numeric vector}
    \item{\code{date}}{a Date}
    \item{\code{const}}{a numeric vector}
    \item{\code{w_DLn}}{a numeric vector}
    \item{\code{n_DLn}}{a numeric vector}
    \item{\code{w_DLn_L1}}{a numeric vector}
    \item{\code{w_DLn_L2}}{a numeric vector}
    \item{\code{w_DLn_L3}}{a numeric vector}
    \item{\code{w_DLn_L4}}{a numeric vector}
    \item{\code{w_DLn_L5}}{a numeric vector}
    \item{\code{w_DLn_L6}}{a numeric vector}
    \item{\code{w_DLn_L7}}{a numeric vector}
    \item{\code{w_DLn_L8}}{a numeric vector}
    \item{\code{n_DLn_L1}}{a numeric vector}
    \item{\code{n_DLn_L2}}{a numeric vector}
    \item{\code{n_DLn_L3}}{a numeric vector}
    \item{\code{n_DLn_L4}}{a numeric vector}
    \item{\code{n_DLn_L5}}{a numeric vector}
    \item{\code{n_DLn_L6}}{a numeric vector}
    \item{\code{n_DLn_L7}}{a numeric vector}
    \item{\code{n_DLn_L8}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
Vintages were downloaded in August, 2014 from FRED
  \describe{
      \item{\code{w}}{Nonfarm Business Sector: Real Compensation Per Hour (COMPRNFB)}
      \item{\code{n}}{All Employees: Total Nonfarm Payrolls (PAYEMS)}
  }
Growth rates are expressed as 100 times the natural log of the level time series.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Data were downloaded from the MATLAB file 'labor_data.mat' available at the home page of
Christiane Baumeister.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(dataLabourMarket)
## maybe str(dataLabourMarket) ; plot(dataLabourMarket) ...
}
\keyword{datasets}
