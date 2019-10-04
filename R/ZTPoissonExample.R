#' ZTPoissonExample
#'
#' Poisson example using VGAM
#'
#' @return Nothing
#' @export
#' @importFrom VGAM vglm pospoisson
#' @importFrom magrittr %>%
#'
#' @examples{
#' ZTPoissonExample()
#' }
ZTPoissonExample <- function() {
  hospitalstay.data <- AdvancedRegression::hospital_stay

  #eliminating zeros from the original data set
  hospitaldays.data <- hospitalstay.data[ which(hospitalstay.data$days != 0), ]

  #fitting zero-truncated Poisson model
  fitted.model <- VGAM::vglm(days ~ gender + age + illness,
                             data = hospitaldays.data,
                             family = VGAM::pospoisson())
  summary(fitted.model) %>% print()

  #checking model fit
  intercept.only.model <- VGAM::vglm(days ~ 1, data = hospitaldays.data,
                                     family = VGAM::pospoisson())

  deviance_pvalue(intercept.only.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   data.frame(gender = "M", age = 55, illness = "no"),
                   type = "response")
}
