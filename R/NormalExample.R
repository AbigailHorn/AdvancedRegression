#' NormalExample
#'
#' Create a linear regression based on \code{job satisfaction} data
#'
#' @return Nothing
#' @export
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom rcompanion plotNormalHistogram
#' @importFrom tibble tibble
#'
#' @examples
#' {
#' NormalExample()
#' }
NormalExample <- function() {
  job.satisfaction.data <- AdvancedRegression::job_satisfaction

  #plotting histogram with fitted normal density
  rcompanion::plotNormalHistogram(job.satisfaction.data$score)

  #testing normality of distribution
  shapiro.test(job.satisfaction.data$score) %>% print()

  #specifying reference levels
  educ.rel <- relevel(job.satisfaction.data$educ, ref = "master")
  gender.rel <- relevel(job.satisfaction.data$gender, ref = "F")

  #fitting general linear model
  fitted.model <- glm(score ~ gender.rel + age + educ.rel,
                      data = job.satisfaction.data,
                      family = gaussian(link = identity))
  summary(fitted.model) %>% print()

  #outputting estimated sigma
  paste0('sigma: ', sigma(fitted.model)) %>% print()

  #checking model fit
  intercept.only.model <- glm(score ~ 1, data = job.satisfaction.data,
                              family = gaussian(link = identity))
  deviance_pvalue(intercept.only.model, fitted.model, df = 4)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   tibble::tibble(gender.rel = "F", age = 40, educ.rel = "bachelor"))
}
