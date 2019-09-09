# Create a tibble with all the chapters and exercise that we have dataset for
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


# Load the CSV file
load_csv <- function(chapter, exercise) {
  readr::read_csv(file = get_csv_filepath(chapter, exercise),
                  col_types = get_csv_cols(chapter, exercise),
                  na = get_csv_nas(chapter, exercise))
}


# create the filepath for the raw CSV file
get_csv_filepath <- function(chapter, exercise, path_base = file.path('.', 'inst', 'csv')){

  chapter_str = paste('chapter',
                      stringr::str_pad(chapter, width = 2, side = 'left', pad = '0'),
                      sep = '_')
  exercise_str = paste('Exercise', chapter, '.', exercise, 'Data.csv', sep = '')
  file.path(path_base, chapter_str, exercise_str)
}


# Create a RDA file for a chapter and exercise
save_data <- function(chapter, exercise) {
  name <- paste0('c', chapter, 'e', exercise)
  assign(name, load_csv(chapter, exercise))
  file_name <- file.path('data', paste(name, 'rda', sep = '.'))
  save(list = name, file = file_name)
  invisible()
}

# Batch make the RDA files
make_data <- function(df = get_csv_chapter_exercise()) {
  purrr::map2(df$chapter, df$exercise, save_data)
  invisible()
}


