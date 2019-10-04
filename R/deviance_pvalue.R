#' deviance_pvale
#'
#' Given a reference model and the fitted model and the degrees of freedom
#' compute the deviance and the p-value. Then print them.
#'
#' @param reference_model The model to act as the reference
#' @param fitted_model The model that you want to compare to the reference model
#' @param df degrees of freedom
#' @param lower.tail Compute the p-value using the lower tail. Default = FALSE (optional)
#' @importFrom stats pchisq
#' @return Nothing
#' @export
#'
#' @examples
#' {
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
#' deviance_pvalue(intercept.only.model, fitted.model, df = 3)
#' }
deviance_pvalue <- function(reference_model, fitted_model, df, lower.tail = FALSE) {
  dev <- deviance(reference_model, fitted_model)
  print(paste0('Deviance: ', dev))
  p.value <- pchisq(dev, df = df, lower.tail = lower.tail)
  print(paste0('p-value: ', p.value))
}