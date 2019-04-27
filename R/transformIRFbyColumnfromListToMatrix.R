#' transformIRFbyColumnFromListToMatrix
#'
#' Auxiliary function that extracts responses of the variables to one shock
#'
#' @param listX list of square n x n matrices of dimension h
#' @param labelShock a numeric scalar, a column index to extract.
#' @param n a numeric scalar,
#' @return matrix h x n, each jth column of a hth list element corresponds to the hth row

transformIRFbyColumnFromListToMatrix <- function( listX, labelShock, n ){

  listX %>% map_df( ~ .x[ , labelShock] ) %>%
    as.matrix %>%
    set_rownames( paste0( "resp", paste0("Y", seq(n) ), "shockY", labelShock ) ) %>%
    t

}
