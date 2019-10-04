#' LongitudinalExample_1
#'
#' Create a repeated measures regression using the \code{cholesterol} data
#'
#' @return Nothing
#' @export
#' @import stats
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom rcompanion plotNormalHistogram
#' @importFrom nlme lme intervals
#' @examples
#' {
#' LongitudinalExample_1()
#' }
LongitudinalExample_1 <- function() {
  cholesterol.data <- AdvancedRegression::cholesterol

  #creating long-form data set
  longform.data <- reshape2::melt(cholesterol.data,
                                  id.vars = c("id", "gender", "age"),
                                  variable.name = "LDLmonth",
                                  value.name = "LDL")

  #creating numeric variable for time
  month <- ifelse(longform.data$LDLmonth == "LDL0", 0,
                  ifelse(longform.data$LDLmonth == "LDL6", 6,
                         ifelse(longform.data$LDLmonth == "LDL9", 9, 24)))

  #plotting histogram with fitted normal density
  rcompanion::plotNormalHistogram(longform.data$LDL)

  #testing for normality of distribution
  shapiro.test(longform.data$LDL) %>% print()

  #specifying reference category
  gender.rel <- relevel(longform.data$gender, ref = "M")

  #fitting random slope and intercept model
  fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
                            random =~ 1 + month|id, data = longform.data)
  nlme::intervals(fitted.model) %>% print()
  summary(fitted.model) %>% print()

  #computing AICC
  n <- 108
  p <- 8
  paste0('AICC: ', AICC(fitted.model, n, p)) %>% print()

  #checking model fit
  null.model <- glm(LDL ~ gender.rel + age + month,
                    data = longform.data)
  deviance_pvalue(null.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   data.frame(gender.rel = "F", age = 48, month = 3),
                   level = 0)

}
