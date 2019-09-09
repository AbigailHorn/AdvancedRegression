# Get the col() information for reading CSV files
get_csv_cols <- function(chapter, exercise) {
  switch(chapter,
    get_csv_cols_chapter_1(exercise),
    get_csv_cols_chapter_2(exercise),
    get_csv_cols_chapter_3(exercise),
    get_csv_cols_chapter_4(exercise),
    get_csv_cols_chapter_5(exercise),
    get_csv_cols_chapter_6(exercise),
    get_csv_cols_chapter_7(exercise),
    get_csv_cols_chapter_8(exercise),
    get_csv_cols_chapter_9(exercise),
    get_csv_cols_chapter_10(exercise),
  )
}


get_csv_cols_chapter_1 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 1, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      drug = readr::col_factor(),
      age = readr::col_integer(),
      gender = readr::col_factor(),
      EWL = readr::col_double()
    ),
    readr::cols( # 3
      bodystyle = readr::col_character(),
      country = readr::col_factor(),
      hwy = readr::col_factor(),
      doors = readr::col_integer(),
      leather = readr::col_factor(),
      price = readr::col_integer()
    ),
    readr::cols( # 4
      Age = readr::col_integer(),
      Gender = readr::col_factor(),
      QuietTime = readr::col_integer(),
      NChildren = readr::col_integer(),
      StressLevel = readr::col_integer(),
      JobStatus = readr::col_factor(),
      NActivities = readr::col_integer(),
      PastVac = readr::col_integer(),
      Sleephours = readr::col_double()
    ),
    readr::cols( # 5
      age = readr::col_integer(),
      gender = readr::col_factor(),
      run = readr::col_double(),
      t1 = readr::col_double(),
      bike = readr::col_double(),
      t2 = readr::col_double(),
      swim = readr::col_double()
    ),
    readr::cols( # 6
      age = readr::col_integer(),
      gender = readr::col_factor(),
      ethnicity = readr::col_factor(),
      BMI = readr::col_double(),
      nmeds = readr::col_integer(),
      AQI = readr::col_factor(),
      HR = readr::col_integer()
    )
  )
}


get_csv_cols_chapter_2 <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      gender = readr::col_factor(),
      age = readr::col_integer(),
      group = readr::col_factor(),
      preBMI = readr::col_double(),
      postBMI = readr::col_double()
    ),
    readr::cols( # 2
      designation = readr::col_factor(),
      work_years = readr::col_integer(),
      priorQI = readr::col_factor(),
      score = readr::col_integer()
    ),
    readr::cols( # 3
      gender = readr::col_factor(),
      prior_exp = readr::col_factor(),
      self_eval = readr::col_integer(),
      distance = readr::col_double()
    ),
    readr::cols( # 4
      npolicies = readr::col_integer(),
      years_with_firm = readr::col_integer(),
      open_claims = readr::col_integer(),
      claim_amount = readr::col_double()
    )
  )
}


get_csv_cols_chapter_3 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 3, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      gender = readr::col_factor(),
      age = readr::col_integer(),
      meds = readr::col_factor(),
      response = readr::col_factor()
    ),
    readr::cols( # 3
      success = readr::col_factor(),
      cover = readr::col_factor(),
      methods = readr::col_factor(),
      novels = readr::col_factor(),
      years = readr::col_integer()
    ),
    readr::cols( # 4
      LTV = readr::col_integer(),
      Age = readr::col_integer(),
      Income = readr::col_factor(),
      Default = readr::col_factor()
    ),
    readr::cols( # 5
      group = readr::col_factor(),
      A = readr::col_integer(),
      W = readr::col_integer()
    )
  )
}


get_csv_cols_chapter_4 <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      GPA = readr::col_double(),
      GMAT = readr::col_integer(),
      status = readr::col_factor()
    ),
    readr::cols( # 2
      subscribed = readr::col_integer(),
      magazine = readr::col_factor(),
      resolved = readr::col_factor(),
      satisf = readr::col_integer()
    ),
    readr::cols( # 3
      inbusiness = readr::col_factor(),
      firsttime = readr::col_factor(),
      type = readr::col_factor(),
      amount = readr::col_integer()
    ),
    readr::cols( # 4
      elevation = readr::col_integer(),
      water = readr::col_factor(),
      wind_dir = readr::col_integer(),
      wind_speed = readr::col_integer(),
      outcome = readr::col_factor()
    ),
    readr::cols( # 5
      age = readr::col_integer(),
      gender = readr::col_factor(),
      condition = readr::col_factor()
    ),
    readr::cols( # 6
      status = readr::col_factor(),
      agediff = readr::col_integer(),
      heightdiff = readr::col_integer(),
      drinking = readr::col_factor()
    )
  )
}


get_csv_cols_chapter_5 <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      defectives = readr::col_integer(),
      experience = readr::col_double(),
      shift = readr::col_factor()
    ),
    readr::cols( # 2
      accidents = readr::col_integer(),
      gender = readr::col_factor(),
      age = readr::col_integer(),
      miles = readr::col_integer()
    ),
    readr::cols( # 3
      calls = readr::col_integer(),
      time = readr::col_factor(),
      wind = readr::col_integer(),
      water = readr::col_factor()
    ),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    readr::cols( # 8
      nruns = readr::col_integer(),
      gender = readr::col_factor(),
      age = readr::col_integer(),
      run = readr::col_factor(),
      pace = readr::col_double()
    ),
    readr::cols( # 9
      grade = readr::col_factor(),
      hw = readr::col_factor(),
      gender = readr::col_factor(),
      books = readr::col_integer()
    ),
    readr::cols( # 10
      BMI = readr::col_double(),
      age = readr::col_integer(),
      gender = readr::col_factor(),
      smoking = readr::col_factor(),
      attacks = readr::col_integer()
    ),
    stop(paste('chapter 5, exercise', exercise, 'is invalid')),
    readr::cols( # 12
      comps = readr::col_integer(),
      books = readr::col_double(),
      jrnls = readr::col_integer(),
      budget = readr::col_double()
    ),
    readr::cols( # 13
      daysnomeds = readr::col_integer(),
      gender = readr::col_factor(),
      age = readr::col_integer(),
      othermeds = readr::col_integer()
    )
  )
}


get_csv_cols_chapter_6 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 6, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      max_temp = readr::col_integer(),
      min_temp = readr::col_integer(),
      feeding_level = readr::col_factor(),
      ndead_mussels = readr::col_integer()
    ),
    readr::cols( # 3
      age = readr::col_integer(),
      gender = readr::col_factor(),
      job = readr::col_factor(),
      allowance = readr::col_integer()
    ),
    readr::cols( # 4
      nkayaks = readr::col_integer(),
      party_size = readr::col_integer(),
      route_length = readr::col_integer(),
      camped = readr::col_factor()
    ),
    readr::cols( # 5
      nnewvideos = readr::col_integer(),
      nvideos = readr::col_integer(),
      nsubscr = readr::col_double(),
      nviews = readr::col_double(),
      type = readr::col_factor()
    ),
    readr::cols( # 6
      nclaimspast5ys = readr::col_integer(),
      nclaimsprev5ys = readr::col_integer(),
      age = readr::col_integer(),
      gender = readr::col_factor()
    ),
    readr::cols( # 7
      DMFTindex = readr::col_integer(),
      age = readr::col_integer(),
      gender = readr::col_factor(),
      oral_hygiene = readr::col_factor()
    ),
    stop(paste('chapter 6, exercise', exercise, 'is invalid')),
    readr::cols( # 9
      ngameinjuries = readr::col_integer(),
      gender = readr::col_factor(),
      nsports = readr::col_integer(),
      npracticeinjuries = readr::col_integer()
    )
  )
}

get_csv_cols_chapter_7 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 7, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      mass = readr::col_integer(),
      wingspan = readr::col_integer(),
      distance = readr::col_integer(),
      nringed = readr::col_integer(),
      nmigrated = readr::col_integer()
    ),
    readr::cols( # 3
      perc_hospzd = readr::col_integer(),
      hosp_loc = readr::col_factor(),
      hosp_type = readr::col_factor(),
      nbeds = readr::col_integer()
    ),
    readr::cols( # 4
      distance = readr::col_integer(),
      method = readr::col_factor(),
      depth = readr::col_integer(),
      percbycatch = readr::col_integer()
    ),
    readr::cols( # 5
      perc_sold = readr::col_double(),
      avg_price = readr::col_double(),
      nhouses = readr::col_integer(),
      age = readr::col_integer()
    ),
    readr::cols( # 6
      trophies = readr::col_integer(),
      firstplaces = readr::col_integer(),
      years = readr::col_integer(),
      blackbelts = readr::col_integer(),
      pupils = readr::col_integer()
    ),
    readr::cols( # 7
      nplanted = readr::col_integer(),
      nsurvived = readr::col_integer(),
      pestcontrol = readr::col_integer(),
      fertilization = readr::col_integer(),
      precipitation = readr::col_integer(),
      windspeed = readr::col_double()
    ),
    readr::cols( # 8
      gender = readr::col_factor(),
      expyr = readr::col_integer(),
      bonus = readr::col_double(),
      propsales = readr::col_double()
    ),
    readr::cols( # 9
      EC = readr::col_double(),
      soiltemp = readr::col_integer(),
      altitude = readr::col_integer(),
      germrate = readr::col_double()
    ),
    readr::cols( # 10
      BMI = readr::col_double(),
      fortyyd = readr::col_double(),
      vertical = readr::col_double(),
      broad = readr::col_integer(),
      bench = readr::col_integer(),
      propgames = readr::col_double()
    )
  )
}


get_csv_cols_chapter_8 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 8, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      id = readr::col_factor(),
      totalyears = readr::col_integer(),
      status = readr::col_factor(),
      bonus15 = readr::col_integer(),
      bonus16 = readr::col_integer(),
      bonus17 = readr::col_integer()
    ),
    readr::cols( # 3
      id = readr::col_factor(),
      gender = readr::col_factor(),
      age = readr::col_integer(),
      doctor = readr::col_factor(),
      length1 = readr::col_integer(),
      length2 = readr::col_integer(),
      length3 = readr::col_integer(),
      score1 = readr::col_double(),
      score2 = readr::col_double(),
      score3 = readr::col_double()
    ),
    readr::cols( # 4
      id = readr::col_factor(),
      gender = readr::col_factor(),
      age = readr::col_integer(),
      oxygen1 = readr::col_double(),
      runtime1 = readr::col_double(),
      pulse1 = readr::col_integer(),
      oxygen2 = readr::col_double(),
      runtime2 = readr::col_double(),
      pulse2 = readr::col_integer(),
      oxygen3 = readr::col_double(),
      runtime3 = readr::col_double(),
      pulse3 = readr::col_integer()
    ),
    readr::cols( # 5
      id = readr::col_factor(),
      group = readr::col_factor(),
      gender = readr::col_factor(),
      aexercise = readr::col_integer(),
      aBMI = readr::col_double(),
      bexercise = readr::col_integer(),
      bBMI = readr::col_double(),
      cexercise = readr::col_integer(),
      cBMI = readr::col_double()
    )
  )
}


get_csv_cols_chapter_9 <- function(exercise) {
  switch(exercise,
    readr::cols( # 1
      patid = readr::col_factor(),
      group = readr::col_factor(),
      gender = readr::col_factor(),
      EWL1 = readr::col_double(),
      EWL2 = readr::col_double(),
      EWL3 = readr::col_double(),
      EWL4 = readr::col_double()
    ),
    readr::cols( # 2
      id = readr::col_factor(),
      group = readr::col_factor(),
      gender = readr::col_factor(),
      afruits = readr::col_integer(),
      aexercise = readr::col_integer(),
      bfruits = readr::col_integer(),
      bexercise = readr::col_integer(),
      cfruits = readr::col_integer(),
      cexercise = readr::col_integer(),
      dfruits = readr::col_integer(),
      dexercise = readr::col_integer()
    ),
    readr::cols( # 3
      patid = readr::col_factor(),
      dosage = readr::col_factor(),
      gender = readr::col_factor(),
      week1 = readr::col_logical(),
      week3 = readr::col_logical(),
      week7 = readr::col_logical(),
      week16 = readr::col_logical()
    ),
    stop(paste('chapter 9, exercise', exercise, 'is invalid')),
    readr::cols( # 5
      Hotel = readr::col_factor(),
      Region = readr::col_factor(),
      ADR1 = readr::col_integer(),
      OCR1 = readr::col_integer(),
      ADR2 = readr::col_integer(),
      OCR2 = readr::col_integer(),
      ADR3 = readr::col_integer(),
      OCR3 = readr::col_integer(),
      ADR4 = readr::col_integer(),
      OCR4 = readr::col_integer()
    ),
    readr::cols( # 6
      id = readr::col_factor(),
      gender = readr::col_factor(),
      age = readr::col_factor(),
      edu = readr::col_factor(),
      pdc1 = readr::col_double(),
      pdc2 = readr::col_double(),
      pdc3 = readr::col_double(),
      pdc4 = readr::col_double()
    )
  )
}


get_csv_cols_chapter_10 <- function(exercise) {
  switch(exercise,
    stop(paste('chapter 10, exercise', exercise, 'is invalid')),
    readr::cols( # 2
      school = readr::col_factor(),
      API = readr::col_integer(),
      subject = readr::col_factor(),
      classsize = readr::col_integer(),
      year = readr::col_integer(),
      score = readr::col_double()
    ),
    readr::cols( # 3
      state = readr::col_factor(),
      county = readr::col_factor(),
      township = readr::col_factor(),
      popl = readr::col_double(),
      pest = readr::col_factor(),
      pm2_5 = readr::col_double()
    ),
    readr::cols( # 4
      portfolio = readr::col_factor(),
      asset = readr::col_factor(),
      type = readr::col_factor(),
      day1 = readr::col_logical(),
      day2 = readr::col_logical(),
      day3 = readr::col_logical(),
      day4 = readr::col_logical(),
      day5 = readr::col_logical()
    ),
    readr::cols( # 5
      school = readr::col_factor(),
      class = readr::col_factor(),
      student = readr::col_factor(),
      gender = readr::col_factor(),
      task1 = readr::col_integer(),
      task2 = readr::col_integer(),
      task3 = readr::col_integer(),
      task4 = readr::col_integer()
    ),
    readr::cols( # 6
      univ = readr::col_factor(),
      dept = readr::col_factor(),
      year1 = readr::col_integer(),
      year2 = readr::col_integer(),
      year3 = readr::col_integer()
    ),
    readr::cols( # 7
      center = readr::col_factor(),
      subject = readr::col_factor(),
      gender = readr::col_factor(),
      medA = readr::col_double(),
      medB = readr::col_double(),
      medC = readr::col_double(),
      medD = readr::col_double()
    )
  )
}

