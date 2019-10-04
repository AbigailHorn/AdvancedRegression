#' GammaExample
#'
#' Using the \code{real estate} data, perform a regression using
#' a box-cox transformation
#'
#' @return Nothing
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @import stats
#'
#' @examples{
#' GammaExample()
#' }
GammaExample <- function() {

  real.estate.data <- AdvancedRegression::real_estate

  #rescaling variables and specifying reference categories
  price10K <- real.estate.data$price / 10000
  sqftK <- real.estate.data$sqft / 1000
  heating.rel <- relevel(real.estate.data$heating, ref = "none")
  AC.rel <- relevel(real.estate.data$AC, ref = "no")
  lotK <- real.estate.data$lot / 1000

  #fitting gamma regression
  fitted.model <- glm(price10K ~ beds + baths + sqftK + heating.rel + AC.rel + lotK,
                      data = real.estate.data, family = stats::Gamma(link = log))
  summary(fitted.model) %>% print()

  #checking model fit
  intercept.only.model <- glm(price10K ~ 1, family = stats::Gamma(link = log))
  deviance_pvalue(intercept.only.model, fitted.model, df = 7)

  #using fitted model for prediction
  predict_data <- tibble::tibble(beds = 4, baths = 2, sqftK = 1.68,
                                 heating.rel = "central",  AC.rel = "no", lotK = 5)

  prediction <- 10000 * predict(fitted.model, type = "response", predict_data)
  print(predict_data)
  paste0('Prediction: ', prediction) %>% print()
}
