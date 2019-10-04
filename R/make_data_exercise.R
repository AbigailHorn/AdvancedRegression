#' get_csv_chapter_example
#'
#' Create a tibble with all the chapters and exercise that we have dataset for
get_csv_chapter_exercise <- function() {
  dplyr::tibble(
    chapter = c(
      rep(1, 5),
      rep(2, 4),
      rep(3, 4),
      rep(4, 6),
      rep(5, 8),
      rep(6, 7),
      rep(7, 9),
      rep(8, 4),
      rep(9, 5),
      rep(10, 6)
    ),
    exercise = c(
      2:6, #1
      1:4, #2
      2:5, #3
      1:6, #4
      1:3, 8:10, 12:13, #5
      2:7, 9, #6
      2:10, #7
      2:5, #8
      1:3, 5:6, #9
      2:7 #10
    )
  )
}



#' load_csv_exercise
#'
#' Load the CSV file
#' @param chapter Chapter to load
#' @param exercise Exercise in the chapter to load
#' @importFrom readr read_csv
#' @return Nothing
load_csv_exercise <- function(chapter, exercise) {
  readr::read_csv(file = get_csv_filepath_execise(chapter, exercise),
                  col_types = get_csv_cols_exercise(chapter, exercise),
                  na = get_csv_nas_exercise(chapter, exercise))
}


#' get_csv_filepath_execise
#'
#' create the filepath for the raw CSV file
#' @param chapter Chapter to create the file path for
#' @param exercise Exercise to create the file path for
#' @param path_base Base path
#' @importFrom stringr str_pad
#'
get_csv_filepath_execise <- function(chapter, exercise,
                                     path_base = file.path('.', 'inst', 'exercise')){

  chapter_str = paste('chapter',
                      stringr::str_pad(chapter, width = 2, side = 'left', pad = '0'),
                      sep = '_')
  exercise_str = paste('Exercise', chapter, '.', exercise, 'Data.csv', sep = '')
  file.path(path_base, chapter_str, exercise_str)
}


#' save_data_exercise
#' Create a RDA file for a chapter and exercise
#'
#' @param chapter Example chapter number
#' @param exercise Example exercise number
#'
save_data_exercise <- function(chapter, exercise) {
  name <- paste0('exercise_', 'c', chapter, 'e', exercise)
  assign(name, load_csv_exercise(chapter, exercise))
  file_name <- file.path('data', paste(name, 'rda', sep = '.'))
  save(list = name, file = file_name)
  invisible()
}

#' make_data_exercise
#'
#' Batch make the RDA files
#'
#' @param df data.frame to use
#' @importFrom purrr map2
make_data_exercise <- function(df = get_csv_chapter_exercise()) {
  purrr::map2(df$chapter, df$exercise, save_data_exercise)
  invisible()
}


