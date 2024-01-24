#' @title get_driver_coefs
#' @description Pull key driver coefficients from a fit object using \code{broom::tidy}
#' and creates an tibble that can be exported or used in further analysis.  Will
#' pull similar but slightly different values if the fit is either an lm or glm
#' object.  If an lm object from regression, will pull six columns: term (coefficient
#' name), estimate (regression coefficient), p.value (coefficient p value), rsq
#' (overall model adjusted r-squared), p.mod (overall model F test), and dir_prob
#' (direction problem flag where 1 indicated direction problem, 0 indicates no
#' direction problem).  For dir_pro, variables must end with either \"_HI\" or
#' \"_LO\" suffix and will assume that HI should be positive and LO should be
#' negative.  If a glm object is detected,nine columns are pulled:  term
#' (coefficient name), estimate (glm estimate), or (Odds Ratio calculated from
#' the estimate), prob (probability calculated from the Odds Ration), p.value
#' (coefficient p value), rsq (overall simulated model adjusted r-squared, based
#' on SSE and calculated by the \code{rsq} package), p.mod (overall model F test
#' based on /code{ResourceSelection::hoslem.test}, dir_prob(direction problem flag,
#' identical to meaning from lm object), and plus_10 (simulation of the change in
#' probability of being in the target group if 10% is added to each predictor,
#' one at a time, holding all other values constant or at the mean, regardless
#' if it's numerically possible to add 10% or not).
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
#'  get_driver_coefs(fit1)
#'
#'  mod2 <- qmod::mod1 %>%
#'    mutate(nps = dplyr::case_when(nps==100 ~ 1,
#'                                  nps <100 ~ 0))
#'
#'  fit2 <- glm(data=mod2 , formula=nps~., family="binomial")
#'  get_driver_coefs(fit2)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort
#' @importFrom broom tidy glance
#' @importFrom dplyr mutate case_when select summarize_all bind_cols mutate_at filter pull left_join
#' @importFrom stringr str_detect
#' @importFrom rsq rsq
#' @importFrom ResourceSelection hoslem.test
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr

get_driver_coefs <- function(fit=NULL, type=NULL, print=FALSE){

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
      dplyr::mutate(dir_prob = dplyr::case_when(stringr::str_detect(term, "_HI") ~ ifelse(estimate < 0, 1, 0),
                                             TRUE ~ ifelse(estimate > 0, 1, 0))) %>%
      dplyr::select(term, estimate, p.value, rsq, p.mod, dir_prob)
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
      dplyr::mutate(dir_prob = dplyr::case_when(stringr::str_detect(term, "_HI") ~ ifelse(or < 1, 1, 0),
                                                TRUE ~ ifelse(or >= 1, 1, 0))) %>%
      select(term, estimate, or, prob, p.value, rsq, p.mod, dir_prob)

    if (Sys.getenv("QMOD_TEST")==TRUE){

    } else {
      output <- output %>%
        dplyr::mutate(rsq = ifelse(term=="(Intercept)", suppressWarnings(rsq::rsq(fit, type="sse", adj=TRUE)), NA))
    }

    #Simulations
    means <- fit$data %>%
      dplyr::summarize_all(., mean, na.rm=TRUE)

    pred_dat <- tibble::tibble(scen=1:nrow(output),
                               term=output$term) %>%
      dplyr::bind_cols(purrr::map_dfr(seq_len(nrow(output)), ~means) )


    for (i in 2:nrow(pred_dat)){
      which_col <- pred_dat$term[[i]]

      pred_dat <- pred_dat %>%
        dplyr::mutate_at(which_col, ~ifelse(term==which_col, .+.1, .))
    }

    pred_dat <- pred_dat %>%
      dplyr::mutate(pred = predict(fit, pred_dat, type="response"))

    adj <- dplyr::filter(pred_dat, scen==1) %>%
      dplyr::mutate(calibration = nps - pred) %>%
      dplyr::select(calibration) %>%
      dplyr::pull()

    pred_dat <- pred_dat %>%
      dplyr::mutate(pred = pred + adj) %>%
      dplyr::mutate(plus_10 = round(pred - nps, 3)) %>%
      dplyr::select(term, plus_10)

    output <- dplyr::left_join(output, pred_dat, by="term")
  }

  if (print==TRUE){
    options(pillar.sigfig = 2)
    print(output, n=nrow(output))
  } else {
    options(pillar.sigfig = 2)
    return(output)
  }


}
