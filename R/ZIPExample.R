#' ZIPExample
#'
#' Zero inflated Poison regression using \code{smoking} data with
#' the \code{pscl} package
#'
#' @return Nothing
#' @export
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom pscl zeroinfl
#' @examples
#' {
#' ZIPExample()
#' }
ZIPExample <- function() {
  smoking.data <- AdvancedRegression::smoking

  #specifying reference category
  health.rel <- relevel(smoking.data$health, ref = "good")

  #fitting zero-inflated Poisson model
  fitted.model <- pscl::zeroinfl(cigarettes ~ gender + age|health.rel,
                                 data = smoking.data)
  summary(fitted.model) %>% print()

  #checking model fit
  intercept.only.model <- pscl::zeroinfl(cigarettes ~ 1, data = smoking.data)
  deviance_pvalue(intercept.only.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   data.frame(gender = "M", health.rel = "good", age = 50))
}
