#' LongitudinalExample_2
#'
#' Create a repeated measures regression using the \code{cholesterol} data
#'
#' @return Nothing
#' @export
#' @import stats
#' @importFrom nlme getVarCov lme
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @examples
#' {
#' LongitudinalExample_2()
#' }
LongitudinalExample_2 <- function() {
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

  #specifying reference category
  gender.rel <- relevel(longform.data$gender, ref = "M")

  #fitting random slope and intercept model with
  #unstructured covariance matrix of error terms
  un.fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
                               random =~ 1 + month|id, data = longform.data,
                               correlation = nlme::corSymm(),
                               weights = nlme::varIdent(form =~ id|month))
  summary(un.fitted.model) %>% print()

  nlme::getVarCov(un.fitted.model, type = "conditional") %>% print()

  #computing AICC
  n <- 108
  p <- 17
  paste0('AICC: ', AICC(un.fitted.model, n, p)) %>% print()

  #checking model fit
  null.model <- glm(LDL ~ gender.rel + age + month,
                    data = longform.data, family = gaussian(link = identity))
  summary(null.model) %>% print()

  deviance_pvalue(null.model, un.fitted.model, df = 12)

  #fitting random slope and intercept model with
  #Toeplitz covariance matrix of error terms
  toep.fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
                                 random =~ 1 + month|id, data = longform.data,
                                 correlation = nlme::corARMA(form =~ 1|id, p = 1, q = 1))

  summary(toep.fitted.model) %>% print()
  nlme::getVarCov(toep.fitted.model, type = "conditional") %>% print()

  #computing AICC
  p <- 11
  paste0('AICC: ', AICC(toep.fitted.model, n, p)) %>% print()

  #checking model fit
  deviance_pvalue(null.model, toep.fitted.model, df = 6)

  #fitting random slope and intercept model with
  #spatial power covariance matrix of error terms
  sppow.fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
                                  random =~ 1 + month|id, data = longform.data,
                                  correlation = nlme::corCAR1(form =~ month|id))
  nlme::getVarCov(sppow.fitted.model, type = "conditional") %>% print()
  summary(sppow.fitted.model) %>% print()

  #computing AICC
  p <- 9
  paste0('AICC: ', AICC(un.fitted.model, n, p)) %>% print()

  #checking model fit
  deviance_pvalue(null.model, sppow.fitted.model, df = 4)

  #fitting random slope and intercept model with
  #autoregressive covariance matrix of error terms
  ar1.fitted.model <- nlme::lme(LDL ~ gender.rel + age + month,
                                random =~ 1 + month|id, data=longform.data,
                                correlation = nlme::corAR1(form=~ 1|id))
  summary(ar1.fitted.model) %>% print()
  nlme::getVarCov(ar1.fitted.model, type = "conditional") %>% print()

  #computing AICC
  p <- 9
  paste0('AICC: ', AICC(ar1.fitted.model, n, p)) %>% print()

  #checking model fit
  deviance_pvalue(null.model, ar1.fitted.model, df = 4)

  #fitting random slope and intercept model with
  #compound symmetric covariance matrix of error terms
  cs.fitted.model <- lme(LDL ~ gender.rel + age + month,
                         random =~ 1 + month|id, data = longform.data,
                         correlation = nlme::corCompSymm(form =~ 1|id))
  summary(cs.fitted.model) %>% print()
  nlme::getVarCov(cs.fitted.model, type = "conditional") %>% print()

  #computing AICC
  p <- 9
  paste0('AICC: ', AICC(cs.fitted.model, n, p)) %>% print()

  #checking model fit
  deviance_pvalue(null.model, cs.fitted.model, df = 4)

  #using AR fitted model for prediction
  print_prediction(ar1.fitted.model,
                   data.frame(gender.rel = "F", age = 48, month = 3),
                   level = 0)
}