#' estimateVAR
#'
#' estimates a reduced-form VAR
#'
#' @param db, time tibble: data
#' @param asy, string: names of LHS variates
#' @param asx, string: names of RHS variates
#' @param estmBeg, date: initial period in the estimation sample
#' @param estmEnd, date: last period in the estimation sample
#' @param iIncpt, logical: include intercept on the RHS, default = TRUE
#'
#' @return list
#'
#' @export
#'
#' @examples
estimateVAR <- function( db, asy, asx, estmBeg, estmEnd, iIncpt = TRUE ){

  ### set sample
  estmBeg <- '1970-03-01'
  estmEnd <- '2014-06-01'
  dbEstm  <- filter_time( db, estmBeg ~ estmEnd)

  mY <- dbEstm %>% select(        asy ) %>% as.matrix
  if( iIncpt ){

    mX <- dbEstm %>% select( const, asx ) %>% as.matrix

  }else{

    mX <- dbEstm %>% select( asx ) %>% as.matrix

  }

  ### get RFVAR coefficients
  mVARcoef <- solve( crossprod(mX), crossprod( mX, mY ) )

  ### covariance of residual matrix
  mRes   <- mY - mX %*% mVARcoef
  mSigma <- var( mRes )

  ### remove constant from the varCoef
  mVARcoefOnly <- mVARcoef %>% t %>% .[, -which( colnames(mX) == "const" ) ]

  ### create companion matrix
  mVARcoefComp <- getCompanionForm( mVARcoefOnly )

  return( list( mVARcoef = mVARcoef, mVARcoefComp = mVARcoefComp, mSigma = mSigma, mRes = mRes ) )
}
