globalVariables(c(".", ":=", "x", "packageVersion"))


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
