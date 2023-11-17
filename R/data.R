#' @title \code{raw1} is an example key driver dataset but prior to data processing
#'
#' @description Dataset containing all the necessary variables for running a
#' key driver analysis but prior to any initial data processing.  Once processed,
#' will result in \code{qmod::dat1}.
#'
#' @format A data frame of 10,408 observations and 13 variables:
#' \describe{
#' \item{response_id}{Respondent ID Number}
#' \item{ltr}{Likelihood to Recommend Rating}
#' \item{price}{Satisfaction with Price}
#' \item{billing}{Satisfaction with Billing}
#' \item{payment_options}{Satisfaction with Payment Options}
#' \item{customer_service}{Satisfaction with Customer Service}
#' \item{online_account}{Satisfaction with Online Account}
#' \item{registration_process}{Satisfaction with Registration Process}
#' \item{services_products}{Satisfaction with Services Products}
#' \item{smart_tech}{Satisfaction with Smart Tech}
#' \item{year}{Year indicator}
#' \item{fuel_type}{Indicator of Fuel Type}
#' \item{payment_type}{Indicator of Payment Type}
#' }
#' @details
#'
#' LTR is a score ranging from 0 to 10, to be used to construct an NPS score.
#'
#' All Satisfaction ratings (from price through smart_tech) are based on a 5 point
#' scale where 1 is Very Unsatisfied and 5 is Very Satisfied.
#'
#' Remaining variables, year, fuel_type, and payment_type, are nominal variables
#' used to classify respondents so that missing values can be replaced by the
#' mean of those groups.
"raw1"

#' @title \code{dat1} is an example key driver dataset
#'
#' @description Dataset containing all the necessary variables for running a
#' key driver analysis.  Based on \code{qmod::raw1} but processed to be ready for
#' modeling.
#'
#' @format A data frame of 10,408 observations and 13 variables:
#' \describe{
#' \item{response_id}{Respondent ID Number}
#' \item{nps}{NPS Score}
#' \item{price}{Satisfaction with Price}
#' \item{billing}{Satisfaction with Billing}
#' \item{payment_options}{Satisfaction with Payment Options}
#' \item{customer_service}{Satisfaction with Customer Service}
#' \item{online_account}{Satisfaction with Online Account}
#' \item{registration_process}{Satisfaction with Registration Process}
#' \item{services_products}{Satisfaction with Services Products}
#' \item{smart_tech}{Satisfaction with Smart Tech}
#' }
#' @details
#'
#' NPS score is based on LTR rating (not included) so includes only values of
#' -100, 0, and 100
#'
#' All Satisfaction ratings (from price through smart_tech) are based on a 5 point
#' scale where 1 is Very Unsatisfied and 5 is Very Satisfied.
#'
#' All missing data from the original \code{qmod::raw1} has been replaced with
#' mean values by year, fuel type, and payment type (not included).
"dat1"


