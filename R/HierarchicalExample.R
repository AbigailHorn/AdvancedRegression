#' HierachialExample
#'
#' Creates a Hierachial model using family depression data \code{dyads} and
#' prints the results
#'
#' @return Nothing
#' @export
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom rcompanion plotNormalHistogram
#' @importFrom lme4 lmer
#' @importFrom tibble tibble
#' @import stats
#' @examples
#' {
#' HierarchialExample()
#' }
HierarchialExample <- function() {
  dyads.data <- AdvancedRegression::dyads

  data.depr <- reshape2::melt(
    dyads.data[,c("family", "individual", "relation", "depression1","depression2",
                  "depression3")],
               id.vars = c("family", "individual", "relation"),
               variable.name = "depr.visits",
               value.name = "depression")


  data.qol <- suppressMessages(
    reshape2::melt(dyads.data[,c("qol1", "qol2", "qol3")],
                   variable.name = "qol.visits", value.name = "qol"))

  longform.data <- cbind(data.depr, data.qol)

  #creating numeric variable for time
  visit <- ifelse(longform.data$depr.visits == "depression1", 1,
                  ifelse(longform.data$depr.visits == "depression2", 2, 3))

  #plotting histogram with fitted normal density
  rcompanion::plotNormalHistogram(longform.data$qol)

  #testing for normality of distribution
  shapiro.test(longform.data$qol) %>% print()

  #specifying reference category
  longform.data$depression <- ifelse(longform.data$depression, "1", "0")
  depression.rel <- relevel(as.factor(longform.data$depression), ref = "1")

  #fitting hierarchical model
  fitted.model <- lme4::lmer(qol ~ relation + depression.rel + visit +
                             (1 + visit|family)+ (1 + visit|family:individual),
                             data = longform.data)
  summary(fitted.model) %>% print()

  #checking model fit
  null.model <- glm(qol ~ relation + depression.rel + visit, data = longform.data)

  deviance_pvalue(null.model, fitted.model, df = 5)

  #using fitted model for prediction
  print_prediction(model = fitted.model,
                   predict_data = tibble::tibble(family = 25, individual = 1,
                                  relation = "M",
                                  depression.rel = "0", visit = 3),
                   allow.new.levels = TRUE)
}
