# package libraries ####
library(here)
library(tidyverse)
library(pins)
library(janitor)

# connect to pin board for tidy data within this project ####
report_pb <- board_folder(
  path = "data-tidy"
)

report_pb %>%
  pin_list()

# read downloaded data 1999-2020 ####
# national ####
usa_x_year <- read_tsv(
  file = "data-raw/cdc-wonder-underlying-cause-of-death-1999-2020-request-usa.txt",
  n_max = 10
) %>%
  clean_names()

str(usa_x_year)
glimpse(usa_x_year)

# tidy
usa_x_year <- usa_x_year %>%
  filter(is.na(notes)) %>%
  mutate(
    state = "USA"
  )

# arizona ####
az_x_year <- read_tsv(
  file = "data-raw/cdc-wonder-underlying-cause-of-death-1999-2020-request-az.txt",
  n_max = 511
) %>%
  clean_names()

str(az_x_year)
glimpse(az_x_year)

# transform to contain data for AZ only
az_x_year <- az_x_year %>%
  filter(state == "Arizona" & is.na(notes))

# join ####
cdc_wonder_1999_2020 <- bind_rows(
  usa_x_year,
  az_x_year
) %>%
  rename(
    geography = state
  ) # %>%
# ggplot(
#   mapping = aes(
#     x = year,
#     y = age_adjusted_rate,
#     group = geography
#   )
# ) +
# geom_line(
#   mapping = aes(
#     color = geography
#   )
# ) +
# ylim(0,NA)

# read data for 2018-2021
# arizona
az_2021 <- read_tsv(
  file = "data-raw/cdc-wonder-underlying-cause-of-death-2018-2021-single-race-request-az.txt",
  n_max = 257
) %>%
  clean_names()

# filter-subset
az_2021 <- az_2021 %>%
  filter(state == "Arizona" & is.na(notes))

# usa
usa_2021 <- read_tsv(
  file = "data-raw/cdc-wonder-underlying-cause-of-death-2018-2021-single-race-request-usa.txt",
  n_max = 5
) %>%
  clean_names()

# transform to match az
usa_2021 <- usa_2021 %>%
  filter(is.na(notes)) %>%
  mutate(state = "USA")

# join 2018-2021
cdc_wonder_2012_2021 <- bind_rows(
  usa_2021,
  az_2021
) %>%
  rename(
    geography = state
  ) %>%
  filter(year == 2021) %>%
  bind_rows( # join with 2012-2020
    cdc_wonder_1999_2020
  ) %>%
  arrange(year, geography)

# save to pin
# set pin meta data
p_name <- str_c(
  "cdc-wonder-age-adjusted-rates-suicide-2012-2021"
)

p_title <- str_c(
  "CDC WONDER Age-Adjusted Mortality Rates of Suicide, USA & AZ, 2012-2021"
)

p_description <- str_c(
  "Citation: Centers for Disease Control and Prevention, National Center for Health Statistics. National Vital Statistics System, Mortality 1999-2020 on CDC WONDER Online Database, released in 2021. Data are from the Multiple Cause of Death Files, 1999-2020, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10.html on Mar 30, 2023 1:45:11 PM"
)

pin_write(
  board = report_pb,
  x = cdc_wonder_2012_2021,
  name = p_name,
  type = "rds",
  title = p_title,
  description = p_description,
  metadata = list(
    user = "rherrera",
    owner = "Coconino HHS",
    department = "epidemiology",
    url = "https://wonder.cdc.gov/"
  )
)
