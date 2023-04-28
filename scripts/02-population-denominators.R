# package libraries ####
library(here)
library(tidyverse)
library(pins)
library(curl)
library(readxl)
library(lubridate)

# connect to local pin board
report_pb <- board_folder(
  path = "data-tidy"
)

report_pb %>%
  pin_list()

# ADHS population denominators for age and sex ####
# source of data for standard population is: https://seer.cancer.gov/stdpopulations/
# Read data ####
us_std_pop <- read_fwf(
  "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  fwf_cols(
    standard = c(1, 3),
    age = c(4, 6),
    standard_pop = c(7, 14)
  )
)

# inspect data
str(us_std_pop)
glimpse(us_std_pop)

# code the data
# Tidy ####
# us standard population
us_std_pop <- us_std_pop %>%
  filter(standard == "204") %>%
  mutate(
    standard = case_when( # recode values
      standard == "204" ~ "2000 U.S. Std Population (18 age groups - Census P25-1130)",
      TRUE ~ as.character(standard)
    ),
    age = case_when( # criteria ~ new value
      age == "000" ~ "0 years",
      age == "001" ~ "0-4 years",
      age == "002" ~ "5-9 years",
      age == "003" ~ "10-14 years",
      age == "004" ~ "15-19 years",
      age == "005" ~ "20-24 years",
      age == "006" ~ "25-29 years",
      age == "007" ~ "30-34 years",
      age == "008" ~ "35-39 years",
      age == "009" ~ "40-44 years",
      age == "010" ~ "45-49 years",
      age == "011" ~ "50-54 years",
      age == "012" ~ "55-59 years",
      age == "013" ~ "60-64 years",
      age == "014" ~ "65-69 years",
      age == "015" ~ "70-74 years",
      age == "016" ~ "75-79 years",
      age == "017" ~ "80-84 years",
      age == "018" ~ "85+ years",
      TRUE ~ as.character(age)
    ),
    standard_pop = as.numeric(standard_pop)
  ) %>%
  rename(age_group = age)

us_std_pop

# Write to pin board ####
report_pb %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_write(
    x = us_std_pop,
    name = "2000-us-standard-population",
    title = "US Standard population",
    type = "rds",
    description = "US Standard Population - 18 age groups. Standard populations, often referred to as standard millions, are the age distributions used as weights to create age-adjusted statistics. https://seer.cancer.gov/stdpopulations/",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt"
    )
  )

# view the pin metadata ####
report_pb %>%
  pin_list()

# ADHS population estimates ####
# source: https://pub.azdhs.gov/health-stats/menu/info/pop/index.php
# Population of Infants, Children (1-14 Years), Adolescents (15-19 Years),
# Young Adults (20-44 Years), Middle-Aged Adults (45-64 Years),
# and Elderly (65+) by Gender, and County of Residence

# create function to read data
read_azdhs_pop_data <- function(x, y, z) {
  # set url
  url <- x

  # set path to save downloaded file
  path_dest <- "data-raw/"

  # set file name to save downloaded file
  path_file <- y

  # download the file and save to designated path
  curl_download(url, destfile = paste(path_dest, "azdhs_pop_", path_file, sep = ""))

  # read the excel file
  mydata <- read_excel(paste(path_dest, "azdhs_pop_", path_file, sep = ""),
    skip = 4,
    n_max = 48
  ) %>%
    clean_names()

  # tidy the data
  # update columan variable names
  names(mydata) <- c("area", "sex", "<1", "1-14", "15-19", "20-44", "45-64", "65+", "total")

  # make table long and add year variable
  mydata <- mydata %>%
    pivot_longer(
      cols = c(3:9),
      names_to = "age_group",
      values_to = "estimate"
    ) %>%
    mutate(year = as.character(z))

  # add missing area names
  mydata[8:21, 1] <- "Arizona"
  mydata[71:84, 1] <- "Coconino"

  mydata <- mydata %>%
    filter(area == "Arizona" | area == "Coconino")

  mydata
}

# years 2014 - 2021 are xlsx files
years_for_analysis_4 <- as.character(seq(2014, 2021, 1))
years_for_analysis_2 <- as.character(seq(14, 21, 1))

azdhs_pop_by_life_stage_xlsx <- map2(
  .x = years_for_analysis_4,
  .y = years_for_analysis_2,
  .f = ~ read_azdhs_pop_data(
    x = str_c(
      "https://pub.azdhs.gov/health-stats/menu/info/pop/",
      .x,
      "/t10a1_",
      .y,
      ".xlsx"
    ),
    y = str_c(
      .x,
      "_t10a1_",
      .y,
      ".xlsx"
    ),
    z = .x
  )
) %>%
  bind_rows()

# years 2012 - 2014 are xls files
years_for_analysis_4 <- as.character(seq(2012, 2013, 1))
years_for_analysis_2 <- as.character(seq(12, 13, 1))

azdhs_pop_by_life_stage_xls <- map2(
  .x = years_for_analysis_4,
  .y = years_for_analysis_2,
  .f = ~ read_azdhs_pop_data(
    x = str_c(
      "https://pub.azdhs.gov/health-stats/menu/info/pop/",
      .x,
      "/t10a1_",
      .y,
      ".xls"
    ),
    y = str_c(
      .x,
      "_t10a1_",
      .y,
      ".xls"
    ),
    z = .x
  )
) %>%
  bind_rows()

# combine all
azdhs_pop_x_age_sex_2012_2021 <- bind_rows(
  azdhs_pop_by_life_stage_xls,
  azdhs_pop_by_life_stage_xlsx
)

# Write to pin board ####
report_pb %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_write(
    x = azdhs_pop_x_age_sex_2012_2021,
    name = "azdhs-population-denominators-age-sex-2012-2021",
    title = "Population of Infants, Children (1-14 Years), Adolescents (15-19 Years), Young Adults (20-44 Years), Middle-Aged Adults (45-64 Years), and Elderly (65+) by Gender, and County of Residence",
    type = "rds",
    description = "Population denominators developed using finalized population estimates from Arizona Office of Economic Opportunity and the Office of Employment and Population Statistics with the Arizona Department of Administration.",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://pub.azdhs.gov/health-stats/menu/info/pop/index.php"
    )
  )

# ADHS Population denominators for ####
# Population by Five-Year Age Groups, County, Gender, and Race/Ethnicity
# create function
read_azdhs_pop_data_age_race_sex <- function(x, y, z) {
  # set url
  url <- x

  # set path to save downloaded file
  path_dest <- "data-raw/"

  # set file name to save downloaded file
  path_file <- y

  # download the file and save to designated path
  curl_download(url, destfile = paste(path_dest, "azdhs_pop_age_county_gender_", path_file, sep = ""))

  # read the excel file
  mydata <- read_excel(paste(path_dest, "azdhs_pop_age_county_gender_", path_file, sep = "")) %>%
    clean_names()

  # tidy the data
  # update columan variable names
  names(mydata) <- c(
    "area", "race_ethnicity", "sex",
    "<1", "1-4", "5-9",
    "10-14", "15-19",
    "20-24", "25-29",
    "30-34", "35-39",
    "40-44", "45-49",
    "50-54", "55-59",
    "60-64", "65-69",
    "70-74", "75-79",
    "80-84", "85+",
    "total"
  )

  # add missing area names
  mydata[5:21, 1] <- "Arizona"
  mydata[63:79, 1] <- "Coconino"

  # add missing race ethnicity categories
  # for arizona
  mydata[5:6, 2] <- "All groups"
  mydata[8:9, 2] <- "White non-Hispanic"
  mydata[11:12, 2] <- "Hispanic or Latino"
  mydata[14:15, 2] <- "Black or African American"
  mydata[17:18, 2] <- "American Indian or Alaska Native"
  mydata[20:21, 2] <- "Asian or Pacific Islander"

  # for coconino
  mydata[62:64, 2] <- "All groups"
  mydata[66:67, 2] <- "White non-Hispanic"
  mydata[69:70, 2] <- "Hispanic or Latino"
  mydata[72:73, 2] <- "Black or African American"
  mydata[75:76, 2] <- "American Indian or Alaska Native"
  mydata[78:79, 2] <- "Asian or Pacific Islander"

  # make table long and add year variable
  mydata <- mydata %>%
    filter(
      area == "Arizona" | area == "Coconino"
    ) %>%
    pivot_longer(
      cols = c(4:23),
      names_to = "age_group",
      values_to = "estimate"
    ) %>%
    mutate(year = as.character(z))

  mydata
}

# years 2014-2021 are xlsx files
years_for_analysis_4 <- as.character(seq(2014, 2021, 1))
years_for_analysis_2 <- as.character(seq(14, 21, 1))

azdhs_pop_data_age_race_sex_xlsx <- map2(
  .x = years_for_analysis_4,
  .y = years_for_analysis_2,
  .f = ~ read_azdhs_pop_data_age_race_sex(
    x = str_c(
      "https://pub.azdhs.gov/health-stats/menu/info/pop/",
      .x,
      "/t10d3_",
      .y,
      ".xlsx"
    ),
    y = str_c(
      .x,
      "_t10d3_",
      .y,
      ".xlsx"
    ),
    z = .x
  )
) %>%
  bind_rows()

# years 2012-2013 are xls files
years_for_analysis_4 <- as.character(seq(2012, 2013, 1))
years_for_analysis_2 <- as.character(seq(12, 13, 1))

azdhs_pop_data_age_race_sex_xls <- map2(
  .x = years_for_analysis_4,
  .y = years_for_analysis_2,
  .f = ~ read_azdhs_pop_data_age_race_sex(
    x = str_c(
      "https://pub.azdhs.gov/health-stats/menu/info/pop/",
      .x,
      "/t10d3_",
      .y,
      ".xls"
    ),
    y = str_c(
      .x,
      "_t10d3_",
      .y,
      ".xls"
    ),
    z = .x
  )
) %>%
  bind_rows()

# combine all
azdhs_pop_data_age_race_sex <- bind_rows(
  azdhs_pop_data_age_race_sex_xls,
  azdhs_pop_data_age_race_sex_xlsx
) %>%
  mutate(
    estimate = as.integer(estimate)
  )

# Write to pin board ####
report_pb %>% # this creates a new folder 'us_std_pop' at the path shown in the pin metadata
  pin_write(
    x = azdhs_pop_data_age_race_sex,
    name = "azdhs-population-denominators-age-sex-race-2012-2021",
    title = "Population by Five-Year Age Groups, County, Gender, and Race/Ethnicity",
    type = "rds",
    description = "Population denominators developed using finalized population estimates from Arizona Office of Economic Opportunity and the Office of Employment and Population Statistics with the Arizona Department of Administration.",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      url = "https://pub.azdhs.gov/health-stats/menu/info/pop/index.php"
    )
  )

# US Census population estimates ####
