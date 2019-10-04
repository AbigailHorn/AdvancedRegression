#' @rdname get_csv_nas
get_csv_nas_exercise <- function(chapter, exercise) {
  if (chapter == 9 & exercise == 6) {
    c(".", "", "NA")
  } else {
    c("", "NA")
  }
}
