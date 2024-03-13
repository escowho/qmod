#' @title get_coefs
#' @description Pull key models coefficients from a fit object using \code{broom::tidy}
#' and creates an tibble that can be exported or used in further analysis.  Will
#' pull similar but slightly different values if the fit is either an lm or glm
#' object.  If an lm object from regression, will pull five columns: term (coefficient
#' name), estimate (regression coefficient), p.value (coefficient p value), rsq
#' (overall model adjusted r-squared), p.mod (overall model F test).
#' If a glm object is detected, seven columns are pulled:  term
#' (coefficient name), estimate (glm estimate), or (Odds Ratio calculated from
#' the estimate), prob (probability calculated from the Odds Ration), p.value
#' (coefficient p value), rsq (overall simulated model adjusted r-squared, based
#' on SSE and calculated by the \code{rsq} package), p.mod (overall model F test
#' based on /code{ResourceSelection::hoslem.test}.
#' @param fit The name of the lm or glm fit object from which to pull the values.
#' Required.
#' @param type The function will attempt to determine the fit type from the
#' fit object but it is also possible to specify the model type by using a
#' character string of either \"lm\" or \"glm\".  Optional.
#' @param print A logical indicating if the results are to be printed to the screen.
#' The function will return a tibble, which can be printed to the console, but the
#' number of rows will be truncated.  The print option defaults to printing all
#' rows in the dataframe.  Default: FALSE
#' @return A tibble object with the coefficients summarized in tabular format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fit1 <- lm(qmod::mod1, formula=nps~.)
#'  get_coefs(fit1)
#'
#'  mod2 <- qmod::mod1 %>%
#'    mutate(nps = dplyr::case_when(nps==100 ~ 1,
#'                                  nps <100 ~ 0))
#'
#'  fit2 <- glm(data=mod2 , formula=nps~., family="binomial")
#'  get_coefs(fit2)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort
#' @importFrom broom tidy glance
#' @importFrom dplyr mutate select
#' @importFrom ResourceSelection hoslem.test
#' @importFrom rsq rsq

get_coefs <- function(fit=NULL, type=NULL, print=FALSE){

  # Checks ------------------------------------------------------------------

  if (is.null(fit)){
    cli::cli_abort("Fit file from lm or glm not found.")
  }

  if (is.null(type)){
    if ("glm" %in% class(fit)){
      type <- "glm"
    } else {
      if ("lm" %in% class(fit)){
        type <- "lm"
      } else {
        cli::cli_abort("Model type (lm or glm) cannot be determined from supplied fit file.")
      }
    }
  } else {
    if (!type %in% c("glm", "lm")){
      cli::cli_abort("Model type must be either lm or glm) cannot be determined from supplied fit file.")
    }
  }

  # lm function -------------------------------------------------------------

  if (type=="lm"){
    output <- broom::tidy(fit) %>%
      dplyr::mutate(estimate = round(estimate, 3),
                    rsq = ifelse(term=="(Intercept)", round(broom::glance(fit)$adj.r.squared, 3), NA),
                    p.mod = ifelse(term=="(Intercept)", round(broom::glance(fit)$p.value, 2), NA),
                    p.value = round(p.value, 2)) %>%
      dplyr::select(term, estimate, p.value, rsq, p.mod)
  }


  # glm function ------------------------------------------------------------

  if (type=="glm"){

    output <- broom::tidy(fit) %>% mutate(p.value = round(p.value, 2)) %>%
      mutate(estimate = round(estimate, 3),
             or = round(exp(estimate), 3),
             prob = round(or/(1+or), 3),
             rsq = ifelse(term=="(Intercept)", 999, NA),
             p.mod = ifelse(term=="(Intercept)", round(ResourceSelection::hoslem.test(fit$y, fitted(fit))$p.value, 2), NA),
             p.value = round(p.value, 2)) %>%
      select(term, estimate, or, prob, p.value, rsq, p.mod)

    if (Sys.getenv("QMOD_TEST")==TRUE){
      output <- output %>%
        dplyr::mutate(rsq = ifelse(term=="(Intercept)", 999), NA)

    } else {
      output <- output %>%
        dplyr::mutate(rsq = ifelse(term=="(Intercept)", suppressWarnings(rsq::rsq(fit, type="sse", adj=TRUE)), NA))
    }

  }

  if (print==TRUE){
    options(pillar.sigfig = 2)
    print(output, n=nrow(output))
  } else {
    options(pillar.sigfig = 2)
    return(output)
  }


}
