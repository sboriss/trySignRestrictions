#' getIRFunsigned
#'
#' returns IRF proposal without any sign restrictions.
#'
#' @param mComp np x np matrix, companion form of the VAR(p) model
#' @param mSigma n x n covariance matrix of the reduced-form residuals
#' @param h a numeric sequence, 0,1,2,...,h length of IRF
#'
#' @return IRF data.frame (h + 1) x n^2
#' @export
#' @importFrom expm %^%
#'
#' @examples


getIRFunsigned <- function( mComp, mSigma, h ){

  ### generate rotation matrix
  n <- dim( mSigma )[1]
  p <- dim( mComp  )[2] / n

  mRV <- mvnfast::rmvn( n, rep(0, n), diag(n) )

  QR <- qr( mRV );

  Q <- qr.Q( QR ); # cat('OLD: Q\n'); Q
  R <- qr.R( QR ); # cat( 'R\n' ); R

  ### flip sign of columns where R[i,i] < 0: don't see why?
  Q <- diag( R ) %>% sign %>% diag %>% `%*%`( Q, . );# cat('NEW: Q\n'); Q

  mBchol <- mSigma %>% chol; mBchol # NB! mBchol is upper-triangular
  mBprop <- crossprod( mBchol, Q ); #cat('mBprop\n'); mBprop

  J  = cbind( diag( n ), matrix( 0, nrow = n, ncol = n * (p-1) ) )

  wold <- h %>% map( ~ J %*% ( mComp %^% .x ) %*% t( J ) )
  listIRF  <- wold %>%
              map( ~ .x %*% mBprop ) %>%
              set_names( paste0( "h=", h ) )
  # listIRFC <- accumulate( listIRF, `+` ) %>% # from https://www.brodrigues.co/blog/2017-03-24-lesser_known_purrr/
  #   set_names( paste0( "h=", h ) )

  listIRF_respIshockJ  <- seq(n) %>%
                          purrr::map( ~ listIRF  %>% transformIRFbyColumnFromListToMatrix( ., .x, n ) )

  mIRF  <- reduce( listIRF_respIshockJ , cbind )

  return( mIRF )
}
