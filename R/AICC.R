#' AICC
#'
#' Compute the AIC for a model
#'
#' @param model Model to use
#' @param n n value
#' @param p p value
#' @importFrom stats logLik
#' @return Nothing
#' @export
#'
#' @examples
#' {
#' cholesterol.data <- AdvancedRegression::cholesterol
#'
#' #creating long-form data set
#' longform.data <- reshape2::melt(cholesterol.data,
#'                                 id.vars = c("id", "gender", "age"),
#'                                 variable.name = "LDLmonth",
#'                                 value.name = "LDL")
#'
#' #creating numeric variable for time
#' month <- ifelse(longform.data$LDLmonth == "LDL0", 0,
#'                 ifelse(longform.data$LDLmonth == "LDL6", 6,
#'                        ifelse(longform.data$LDLmonth == "LDL9", 9, 24)))
#'
#' #specifying reference category
#' gender.rel <- relevel(longform.data$gender, ref = "M")
#'
#' #fitting random slope and intercept model
#' fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
#'                           random =~ 1 + month|id, data = longform.data)
#' AICC(fitted.model, n = 108, p = 8)
#' }
AICC <- function(model, n, p) {
  -2 * logLik(model) + 2 * p * n / (n - p - 1)
}