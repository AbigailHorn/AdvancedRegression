#' BetaExample
#'
#' Using the \code{libaries} data to do a beta regression
#'
#' @return Nothing
#' @export
#' @importFrom betareg betareg
#' @importFrom magrittr %>%
#' @import stats
#' @examples
#' {
#' BetaExample()
#' }
BetaExample <- function() {

  libraries.data <- AdvancedRegression::libraries

  #specifying reference category
  location.rel <- relevel(libraries.data$location, ref = "rural")

  #fitting beta regression model
  fitted.model <- betareg::betareg(propontime ~ nbooks + ncardholders + location.rel,
                                   data = libraries.data, link = "logit")
  summary(fitted.model) %>% print()

  #checking model fit
  intercept.only.model <- betareg::betareg(propontime ~ 1,
                                           data = libraries.data, link = "logit")

  deviance_pvalue(intercept.only.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   tibble::tibble(nbooks = 15, ncardholders = 2.5,
                                  location.rel = "rural"))
}

