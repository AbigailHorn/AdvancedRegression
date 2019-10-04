#' deviance
#'
#' Given two models, compute the deviance between the two
#'
#' @param reference_model The model to act as the reference
#' @param fitted_model The model that you want to compare to the reference model
#'
#' @return The deviance between the two models
#' @export
#' @importFrom stats logLik
#' @examples{
#' libraries.data <- AdvancedRegression::libraries
#'
#' #specifying reference category
#' location.rel <- relevel(libraries.data$location, ref = "rural")
#'
#' #fitting beta regression model
#' fitted.model <- betareg::betareg(propontime ~ nbooks + ncardholders + location.rel,
#'                                  data = libraries.data, link = "logit")
#'
#' #checking model fit
#' intercept.only.model <- betareg::betareg(propontime ~ 1,
#'                                          data = libraries.data, link = "logit")
#'
#' deviance(intercept.only.model, fitted.model)
#' }
#'
deviance <- function(reference_model, fitted_model) {
  -2 * (logLik(reference_model) - logLik(fitted_model))
}