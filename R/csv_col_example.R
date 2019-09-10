# Get the col() information for reading CSV files
get_csv_cols_example <- function(chapter, exercise) {
  switch(chapter,
    get_csv_cols_chapter_1_example(exercise),
    get_csv_cols_chapter_2_example(exercise),
    get_csv_cols_chapter_3_example(exercise),
    get_csv_cols_chapter_4_example(exercise),
    get_csv_cols_chapter_5_example(exercise),
    get_csv_cols_chapter_6_example(exercise),
    get_csv_cols_chapter_7_example(exercise),
    get_csv_cols_chapter_8_example(exercise),
    get_csv_cols_chapter_9_example(exercise),
    get_csv_cols_chapter_10_example(exercise),
  )
}


get_csv_cols_chapter_1_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      gender = col_factor(),
      age = col_integer(),
      educ = col_factor(),
      score = col_integer()
    )
  )
}


get_csv_cols_chapter_2_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      price = col_integer(),
      beds = col_integer(),
      baths = col_double(),
      sqft = col_integer(),
      heating = col_factor(),
      AC = col_factor(),
      lot = col_integer()
    )
  )
}


get_csv_cols_chapter_3_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      ownership = col_factor(),
      nemployees = col_integer(),
      approach = col_factor()
    )
  )
}


get_csv_cols_chapter_4_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      gender = col_factor(),
      age = col_integer(),
      marital = col_factor(),
      educ = col_factor(levels = c('<HS', 'HSgrad', 'HSgrad+'),
                        ordered = TRUE),
      health = col_factor(levels = c('1poor', '2fair', '3good', '4excellent'),
                          ordered = TRUE)
    ),
    stop(paste('chapter 4, exercise', exercise, 'is invalid')),
    stop(paste('chapter 4, exercise', exercise, 'is invalid')),
    readr::cols( # 4
      ID = col_factor(),
      gender = col_factor(),
      age = col_integer(),
      nteeth = col_integer(),
      choice = col_factor()
    )
  )
}


get_csv_cols_chapter_5_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      days = col_integer(),
      gender = col_factor(),
      age = col_integer(),
      illness = col_factor()
    ),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    readr::cols( # 4
      gender = col_factor(),
      health = col_factor(),
      age = col_integer(),
      cigarettes = col_integer()
    ),
    readr::cols( # 4
      ntextbooks = col_integer(),
      renting = col_integer(),
      aid = col_factor()
    )
  )
}


get_csv_cols_chapter_6_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      gender = col_factor(),
      age = col_integer(),
      firsttime = col_factor(),
      sets = col_integer()
    ),
    stop(paste('chapter 6, exercise', exercise, 'is invalid')),
    readr::cols( # 3
      elevation = col_double(),
      mintemp = col_double(),
      snowyears = col_integer()
    ),
    readr::cols( # 4
      ndelinqaccounts = col_integer(),
      age = col_integer(),
      gender = col_factor(),
      income = col_factor(levels = c('Low', 'High'), ordered = TRUE),
      nunemplyears = col_integer()
    )
  )
}

get_csv_cols_chapter_7_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      nbooks = col_double(),
      ncardholders = col_double(),
      location = col_factor(),
      propontime = col_double()
    ),
    readr::cols( # 2
      ntrips = col_integer(),
      nbiked = col_integer(),
      status = col_factor(),
      gender = col_factor(),
      parking = col_integer(),
      distance = col_integer()
    ),
    readr::cols( # 3
      age = col_integer(),
      gender = col_factor(),
      depression = col_logical(),
      diabetes = col_logical(),
      nmeds = col_integer(),
      pdc = col_double()
    ),
    readr::cols( # 4
      grade = col_factor(levels = 5:8, ordered = TRUE),
      gender = col_factor(),
      mathscore = col_double(),
      propassign = col_double()
    )
  )
}


get_csv_cols_chapter_8_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      id = col_factor(),
      gender = col_factor(),
      age = col_integer(),
      LDL0 = col_integer(),
      LDL6 = col_integer(),
      LDL9 = col_integer(),
      LDL24 = col_integer()
    )

  )
}


get_csv_cols_chapter_9_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      patid = col_factor(),
      group = col_factor(),
      day1 = col_integer(),
      week1 = col_integer(),
      week2 = col_integer(),
      week5 = col_integer(),
      month3 = col_integer()
    )
  )
}


get_csv_cols_chapter_10_example <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      family = col_factor(),
      individual = col_factor(),
      relation = col_factor(),
      depression1 = col_logical(),
      depression2 = col_logical(),
      depression3 = col_logical(),
      qol1 = col_double(),
      qol2 = col_double(),
      qol3 = col_double()
    )
  )
}
