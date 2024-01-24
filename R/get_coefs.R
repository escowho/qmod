

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
