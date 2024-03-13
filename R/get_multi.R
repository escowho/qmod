#' @title get_multi
#' @description Pull key multicollinearity diagnostics, names VIF and Klein's Rule,
#' from \code{mctest::imcdiag}.  It reports the variable, the VIF, variable-specific
#' R-squared value used in Klein's Rule, the overall model R-squared value, the
#' difference in the R-square values, then flags indicating if if multicollinearity
#' is detected from the VIF (vif_detect and klein_detect).
#' @param fit The name of the lm or glm fit object from which to pull the values.
#' Required.
#' @return  A tibble object with the multicollinearity statistics in tabular format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fit1 <- lm(qmod::mod1, formula=nps~.)
#'  get_multi(fit1)
#'  }
#' }
#' @export
#' @importFrom mctest imcdiag
#' @importFrom tibble rownames_to_column
#' @importFrom qpack set_colnames
#' @importFrom dplyr mutate left_join select

get_multi <- function(fit=NULL){

  # Checks ------------------------------------------------------------------

  if (is.null(fit)){
    stop(call. = FALSE, "Fit file from lm function not found.")
  }

  # Function ----------------------------------------------------------------

  vif <- mctest::imcdiag(fit, method="VIF")$idiags %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    qpack::set_colnames(c("var", "vif", "vif_detect")) %>%
    dplyr::mutate(vif = round(vif, 1))

  klein <- mctest::imcdiag(fit, method="Klein")$idiags %>%
    data.frame() %>%
    tibble::rownames_to_column() %>%
    qpack::set_colnames(c("var", "r2", "overall", "diff", "klein_detect")) %>%
    dplyr::mutate(r2 = round(r2, 3),
                  overall = round(overall, 3),
                  diff = round(diff, 3))

  output <- dplyr::left_join(vif, klein, by="var") %>%
    dplyr::select(var, vif, r2, overall, diff, vif_detect, klein_detect)
  return(output)
}
