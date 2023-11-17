#' @title create_hilo
#' @description Creates two dichotomous variables from a single rating scale that
#' indicates if a respondent's answer is a high value or a low value, based on
#' cut-offs defined by parameters.  The original variable will be removed and
#' replaced with two 0,1 variables that are named identically to the original
#' variable number but given the suffix \"_HI\" to represent the high or positive
#' end of the rating scale or the suffix \"_LO\" to represent the low or negative
#' end of the rating scale.
#' @param data Name of the dataframe that contains the rating scales.  Assumes that
#' each column is a rating scale of the same scale and will convert them all using
#' the same cut-off values. Required.
#' @param high Value indicating where the High (\"_HI\") cut-off will be.  If the scale
#' is a 7-point scale where 1 is the lowest and 7 is the highest value, a value
#' of 6 here would combine values of 6 and 7 assigned a value of 1 to indicate
#' that the respondent falls into the High (\"_HI\") category and all other values
#' would be assigned a value of 0 to indicate that the respondent does not fall
#' into the category. Required.
#' @param low Value indicating where the Low (\"_LO\") cut-off will be.  If the scale
#' is a 7-point scale where 1 is the lowest and 7 is the highest value, a value
#' of 2 here would combine values of 1 and 2 assigned a value of 1 to indicate
#' that the respondent falls into the Low (\"_LO\") category and all other values
#' would be assigned a value of 0 to indicate that the respondent does not fall
#' into the category. Required.
#' @return Dataframe containing the names of the scales or variables (column names),
#' one each for the \"_HI\" dichotomy and one for the \"_LO\" dichotomy.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  qmod::dat1 %>%
#'    dplyr::select(price:smart_tech) %>%
#'    create_hilo(., 4, 2)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr case_when mutate_if bind_cols select all_of
#' @importFrom haven zap_labels zap_label
#' @importFrom qpack set_colnames
#' @importFrom magrittr set_colnames

create_hilo <- function(data=NULL, high=NULL, low=NULL){

  # Checks ------------------------------------------------------------------

  if (is.null(data)){
    cli::cli_abort("Data must be specified.")
  }

  if (is.null(high) | is.null(low)){
    cli::cli_abort("Values for both high and low must be specified.")
  }

  # Function ----------------------------------------------------------------

  recode_high <- function(variable, high=high){
    dplyr::case_when(variable >= high ~ 1,
                     .default=0)
  }

  recode_low <- function(variable, low=low){
    dplyr::case_when(variable <= low ~ 1,
                     .default=0)
  }

  dat_h <- data %>%
    haven::zap_labels() %>%
    haven::zap_label() %>%
    dplyr::mutate_if(is.numeric, recode_high, high=high) %>%
    qpack::set_colnames(paste0(names(.), "_HI"))

  dat_l <- data %>%
    haven::zap_labels() %>%
    haven::zap_label() %>%
    dplyr::mutate_if(is.numeric, recode_low, low=low) %>%
    magrittr::set_colnames(paste0(names(.), "_LO"))

  index <- c(rbind(colnames(dat_h), colnames(dat_l)))

  out <- dplyr::bind_cols(dat_h, dat_l) %>%
    dplyr::select(dplyr::all_of(index))

  return(out)
}
