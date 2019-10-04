#' PoissonExample
#'
#' Poisson regression using \code{hospital stay} data
#'
#' @return Nothing
#' @export
#' @importFrom magrittr %>%
#' @import stats
#'
#' @examples
#' {
#' PoissonExample()
#' }
PoissonExample <- function() {
  hospitalstay.data <- AdvancedRegression::hospital_stay

  #fitting Poisson model
  fitted.model <- glm(days ~ gender + age + illness,
                      data = hospitalstay.data, family = poisson(link = log))
  summary(fitted.model) %>% print()

  #checking model fit
  intercept.only.model <- glm(days ~ 1, data = hospitalstay.data,
                              family = poisson(link = log))
  deviance_pvalue(intercept.only.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   data.frame(gender = "M", age = 55, illness = "no"),
                   type = "response")
}
