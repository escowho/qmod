globalVariables(c(".", ":=", "x", "packageVersion",
                  "estimate", "term", "p.value", "p.mod", "dir_prob", "or",
                  "fitted", "prob", "predict", "scen", "nps", "pred", "calibration",
                  "plus_10",
                  "billing_HI", "billing_LO", "customer_service_HI", "customer_service_LO",
                  "nps", "online_account_HI", "online_account_LO", "payment_options_HI",
                  "payment_options_LO", "price_HI", "price_LO", "registration_process_HI",
                  "registration_process_LO", "response_id", "services_products_HI",
                  "services_products_LO", "smart_tech_HI", "smart_tech_LO"))


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Convenience Function to Pull Version
#'
#' @keywords internal

version <- function(){
  packageVersion("qmod")
}


#' @title Refreshes github install
#' @description Convenience function to perform a remotes::install_github for qmod.
#' @param dev A logical to indicate if the development version of qmod should be
#' installed.  Default: FALSE.
#' @return Nothing, updates package(s).
#' @keywords internal
#' @examples
#' \dontrun{
#' qmod:::refresh()
#' qmod:::refresh(dev=TRUE)
#' }
#' @importFrom remotes install_github

refresh <- function(dev=FALSE){

  # Function ----------------------------------------------------------------

  if (dev==TRUE){
    if (Sys.getenv("QMOD_TEST")==TRUE){
      return("update_dev")
    } else {
      invisible(remotes::install_github("mshefferq/qmod", ref="dev"))
    }

 } else {

    if (Sys.getenv("QMOD_TEST")==TRUE){
      return("update_pack")
    } else {
      invisible(remotes::install_github("mshefferq/qmod"))
    }
  }

}
