#' getCompanionForm
#'
#' Constructs companion form from n x np matrix of RFVAR coefficients estimated by OLS.
#'
#' @param mB a n x np matrix of the AR coefficients of the reduced-form VAR
#' (without deterministic terms ). The coefficients are organised as follows:
#' Y1_{t-1}, Y2_{t-1}, Y1_{t-2}, Y2_{t-2}, ..., Y1_{t-p}, Y2_{t-p}.
#'
#'
#' @return np x np companion form matrix
#' @export
#'
#' @examples
getCompanionForm <- function( mB ){

  n  <- dim(mB)[1]; # n
  np <- dim(mB)[2]; # np

  mCdown <- cbind( diag(np - n), matrix(0, nrow = np - n, ncol = n ) )
  mC     <- rbind( mB, mCdown )
  return( mC )

}
