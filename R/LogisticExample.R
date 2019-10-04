#' LogisticExample()
#'
#' Create a logistic model using the \code{companies} data.
#'
#' @return Nothing
#' @export
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @examples
#' {
#' LogisticExample()
#' }
LogisticExample <- function() {

  companies.data <- AdvancedRegression::companies

  #specifying reference categories
  ownership.rel <- relevel(companies.data$ownership, ref = "partner")
  approach.rel <- relevel(companies.data$approach, ref = "comp")

  #fitting logistic model
  fitted.model <- glm(approach.rel ~ ownership.rel + nemployees,
                      data = companies.data,
                      family = binomial(link = logit))
  summary(fitted.model) %>% print()

  #extracting AICC and BIC for fitted model
  p <- 4
  n <- 50
  paste0('AICC: ', AICC(fitted.model, n, p)) %>% print()
  paste0('BIC: ', BIC(fitted.model)) %>% print()

  #checking model fit
  intercept.only.model <- glm(approach.rel ~ 1, family = binomial(link = logit))
  deviance_pvalue(intercept.only.model, fitted.model, df = 3)

  #using fitted model for prediction
  print_prediction(fitted.model,
                   tibble::tibble(ownership.rel = "sole", nemployees = 40),
                   type = "response")

}
