#' convertListToDataFrame
#'
#' Converts list of IRF into a data.frame. Each list element corresponds to one IRF proposal.
#'
#' @param listIN a list of IRF and IRFC, each list element contains two sub-elements named "mIRFdraw" and "mIRFCdraw"
#'
#' @return list of three elements
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
convertListToDataFrame <- function( listIN ){ # either "mIRFdraw" or "mIRFCdraw"

  ### select IRF
  listIRF <- listIN %>%
    map( ~{

      .x %>% as.data.frame %>%
        rownames_to_column( var = "h" ) %>%
        gather( key, value, -h )

    })

  ### relabel value column to avoid warning message when merge list elements in IRF
  listIRF %<>% map2( ., seq( length(listIN) ), ~ .x %>% set_colnames( c( colnames( .x )[-3], paste0( "value", .y ) ) ) )

  ### collect all IRFs in one table
  dfIRF <- listIRF %>% purrr::reduce( merge, by = c("key","h" ) )

  # recode h as ordered factor and reorder dfIRF: otherwise h automatically ordered as h=0,h=1,h=10,...
  dfIRF %<>% mutate( h = factor( h, levels = paste0( "h=", opt$stepIRF ) ) ) %>%
    arrange( key, h )

}
