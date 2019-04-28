#' orderShocksInIRFprop
#'
#' The function is meant to be used in a bivariate SVAR with two-demand and supply-shocks.
#' The function orders shocks based on the impact matrix in such a way that the supply shock
#' (+/-) followed by the demand shock (+/+).
#'
#' @param mIRF (h+1) x 2 matrix of IRF proposal from the QR rotation of the B matrix with BB' = Sigma
#' (reduced-form residual covariance matrix).
#'
#' @return matrix of the same dimension in case shocks could be ordered or NULL in case
#'         shocks could not be identified
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'


orderShocksInIRFprop <- function( mIRF ){

  labelIRF <- c( str_c( "respY", c(1,2),"toSupplyShock" ), str_c(  "respY", c(1,2),"toDemandShock" ) )

  ### check existence of different shocks
  twoShocks <- mIRF %>% head( n = 1 ) %>%
    sign %>%
    rowSums %>%
    abs %>% `==`( ., 2 ); twoShocks

  if( twoShocks ){

     mIRFsignRestr <- mIRF[ 1, c( 1, 3 ) ] %>%
      # detect sign of the 1st, 3rd elements for h = 0
      sign %>%
      # extend the sign to all columns subject to this shock
      rep( each = 2 ) %>%
      matrix( nrow = 1 ) %>%
      # impose the positive sign on the 1st and 3rd elements
      sweep( mIRF, MARGIN = 2, STATS = ., FUN= `/` ) %>%
      # order shocks: supply (+/+), demand(+/-)
      { if( .[ 1, 4] < 0 ) .[ , c( 3, 4, 1, 2 ) ] else . } %>%
      set_colnames( labelIRF )

  }else{

    mIRFsignRestr <- NULL

  }
  mIRFsignRestr
}
