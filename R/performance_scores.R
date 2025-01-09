#' @title performance_scores
#' @description Create dataframe that converts rating scales into three categories,
#' Positive, Neutral, and Negative, appropriate for reporting performance data
#' during a Key Driver Analysis.
#' @param data Name of the dataframe that contains the rating scales.  Assumes that
#' each column is a rating scale of the same scale and will convert them all using
#' the same cut-off values. Required.
#' @param high Value indicating where the Positive cut-off will be.  If the scale
#' is a 7-point scale where 1 is the lowest and 7 is the highest value, a value
#' of 6 here would combine values of 6 and 7 into the Positive Category. Required.
#' @param low Value indicating where the Negative cut-off will be.  If the scale
#' is a 7-point scale where 1 is the lowest and 7 is the highest value, a value
#' of 2 here would combine values of 1 and 2 into the Negative Category.  Any
#' value that doesn't fall into the Positive or Negative category will be included
#' in the Neutral category by default. Required.
#' @return Dataframe containing the names of the scales or variables (column names)
#' and the proportion of the dataset that falls within the three categories defined
#' by the high and low cutoffs.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  qmod::raw1 %>%
#'    dplyr::select(price:smart_tech) %>%
#'    performance_scores(., 4, 2)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr case_when mutate_if rename all_of filter group_by summarize n mutate bind_cols bind_rows
#' @importFrom haven zap_labels zap_label
#' @importFrom rlang enquo
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr set_colnames

performance_scores <- function(data, high, low){

  # Checks ------------------------------------------------------------------

  if (is.null(data)){
    cli::cli_abort("Data must be specified.")
  }

  if (is.null(high) | is.null(low)){
    cli::cli_abort("Values for both high and low must be specified.")
  }

  # Function ----------------------------------------------------------------

  recode_hml <- function(variable, high=high, low=low){
    dplyr::case_when(variable >= high ~ 3,
                     variable <= low ~ 1,
                     is.na(variable) ~ NA_real_,
                     .default=2)
  }

  data <- data %>%
    haven::zap_labels() %>%
    haven::zap_label() %>%
    dplyr::mutate_if(is.numeric, recode_hml, high=high, low=low)

  run_freq <- function(data, variable){
    VARIABLE <- rlang::enquo(variable)
    data %>%
      dplyr::rename(x=dplyr::all_of({{variable}})) %>%
      dplyr::filter(!is.na(x)) %>%
      dplyr::group_by(x) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::mutate(n = n/sum(n)) %>%
      tidyr::pivot_wider(names_from=x, values_from=n) %>%
      dplyr::bind_cols(tibble::tibble(var={{variable}}), .)
  }

  f <- vector(mode="list", length = 0)

  for (var in names(data)){
    f[[var]] <- run_freq(data=data, variable=var)
  }

  dplyr::bind_rows(f) %>%
    magrittr::set_colnames(c("Variable", "Negative", "Neutral", "Positive"))

}
