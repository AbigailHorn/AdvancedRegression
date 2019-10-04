#' print_prediction
#'
#' Given a model and data, print the predicted value.
#'
#' @param model Model used to make the prediction
#' @param predict_data Data used to make the prediciton
#' @param ... Any extra parameters to be passed the \code{predict}
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @import stats
#'
#' @return Nothing
#' @export
#'
#' @examples
#' {
#' job.satisfaction.data <- AdvancedRegression::job_satisfaction
#' #specifying reference levels
#' educ.rel <- relevel(job.satisfaction.data$educ, ref = "master")
#' gender.rel <- relevel(job.satisfaction.data$gender, ref = "F")
#'
#' #fitting general linear model
#' fitted.model <- glm(score ~ gender.rel + age + educ.rel,
#'                     data = job.satisfaction.data,
#'                     family = gaussian(link = identity))
#' #using fitted model for prediction
#' print_prediction(fitted.model,
#'                  tibble::tibble(gender.rel = "F", age = 40, educ.rel = "bachelor"))
#' }
print_prediction <- function(model, predict_data, ...){

  print(predict_data)
  prediction <- predict(model, predict_data, ...)
  paste0('prediction: ', prediction) %>% print()
}
