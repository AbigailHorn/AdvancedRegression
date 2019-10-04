#' get_csv_chapter_example
#'
#' Create a tibble with all the chapters and exercise that we have dataset for
#'
get_csv_chapter_example <- function() {
  dplyr::tibble(
    chapter = c(
      rep(1, 1),
      rep(2, 1),
      rep(3, 1),
      rep(4, 2),
      rep(5, 3),
      rep(6, 3),
      rep(7, 4),
      rep(8, 1),
      rep(9, 1),
      rep(10, 1)
    ),
    exercise = c(
      1, #1
      1, #2
      1, #3
      1,4, #4
      1,3,4, #5
      1,3,4, #6
      1:4, #7
      1, #8
      1, #9
      1 #10
    )
  )
}


#' load_csv_example
#'
#' Load the CSV file
#' @param chapter Chapter to load
#' @param exercise Exercise in the chapter to load
#' @importFrom readr read_csv
#' @return Nothing
load_csv_example <- function(chapter, exercise) {
  readr::read_csv(file = get_csv_filepath_execise(chapter, exercise),
                  col_types = get_csv_cols_example(chapter, exercise),
                  na = get_csv_nas_example(chapter, exercise))
}



#' get_csv_filepath_example
#'
#' create the filepath for the raw CSV file
#' @param chapter Chapter to create the file path for
#' @param exercise Exercise to create the file path for
#' @param path_base Base path
#' @importFrom stringr str_pad
#'
get_csv_filepath_example <- function(chapter, exercise,
                                     path_base = file.path('.', 'inst', 'example')){

  chapter_str = paste('chapter',
                      stringr::str_pad(chapter, width = 2, side = 'left', pad = '0'),
                      sep = '_')
  exercise_str = paste('Example', chapter, '.', exercise, 'Data.csv', sep = '')
  file.path(path_base, chapter_str, exercise_str)
}


#' save_data_example
#' Create a RDA file for a chapter and exercise
#'
#' @param chapter Example chapter number
#' @param exercise Example exercise number
#'
save_data_example <- function(chapter, exercise) {
  name <- paste0('example_', 'c', chapter, 'e', exercise)
  assign(name, load_csv_example(chapter, exercise))
  file_name <- file.path('data', paste(name, 'rda', sep = '.'))
  save(list = name, file = file_name)
  invisible()
}

#' make_data_example
#'
#' Batch make the RDA files
#'
#' @param df data.frame to use
#' @importFrom purrr map2
make_data_example <- function(df = get_csv_chapter_example()) {
  purrr::map2(df$chapter, df$exercise, save_data_example)
  invisible()
}


