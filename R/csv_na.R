# Get the 'NA' for reading CSV files
get_csv_nas <- function(chapter, exercise) {
  if (chapter == 9 & exercise == 6) {
    c(".", "", "NA")
  } else {
    c("", "NA")
  }
}
