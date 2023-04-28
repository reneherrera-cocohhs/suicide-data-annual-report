# package libraries ####
library(here)
library(tidyverse)
library(pins)
library(janitor)
# library(epitools)

# source pin board ####
source(
  file = "scripts/01-setup-pins.R",
  echo = TRUE
)

# read mortality data ####
pin_meta(
  board = mortality_pb,
  name = "mortality-data-tidy-transformed-2010-2023"
)

mortality_df <- pin_read(
  board = mortality_pb,
  name = "mortality-data-tidy-transformed-2010-2023"
)

# inspect
glimpse(mortality_df)

# data prep ####

# create additional age group variable for calculating rates
mortality_df %>%
  select(contains("age_")) %>%
  names()

mortality_df <- mortality_df %>%
  mutate(
    d_age_group_adhs = case_when( # age groups to match adhs
      calc_age < 1 ~ "<1",
      calc_age < 15 ~ "1-14",
      calc_age < 20 ~ "15-19",
      calc_age < 45 ~ "20-44",
      calc_age < 65 ~ "45-64",
      calc_age >= 65 ~ "65+"
    ),
    d_age_group_us_std = case_when( # age groups to match 2000 US Standard Population
      calc_age < 5 ~ "0-4 years",
      calc_age < 10 ~ "5-9 years",
      calc_age < 15 ~ "10-14 years",
      calc_age < 20 ~ "15-19 years",
      calc_age < 25 ~ "20-24 years",
      calc_age < 30 ~ "25-29 years",
      calc_age < 35 ~ "30-34 years",
      calc_age < 40 ~ "35-39 years",
      calc_age < 45 ~ "40-44 years",
      calc_age < 50 ~ "45-49 years",
      calc_age < 55 ~ "50-54 years",
      calc_age < 60 ~ "55-59 years",
      calc_age < 65 ~ "60-64 years",
      calc_age < 70 ~ "65-69 years",
      calc_age < 75 ~ "70-74 years",
      calc_age < 80 ~ "75-79 years",
      calc_age < 85 ~ "80-84 years",
      calc_age >= 85 ~ "85+ years"
    )
  ) %>%
  filter(
    calc_age >= 10
  )

# subset-filter to years of interest 2012-2021 and Coconino resident
mortality_df_12_21 <- mortality_df %>%
  filter(
    death_book_year %in% as.character(seq(2012, 2021, 1))
  ) %>%
  filter(d_county_resident == "resident")

# subset to suicide only
mortality_df_suicide <- mortality_df_12_21 %>%
  filter(d_suicide == 1)

# confirm that geospatial data still exists 
mortality_df %>%
  sample_n(size = 10) %>%
  select(starts_with("n_")) %>%
  glimpse()

mortality_df_12_21 %>%
  sample_n(size = 10) %>%
  select(starts_with("n_")) %>%
  glimpse()


mortality_df_suicide %>%
  sample_n(size = 10) %>%
  select(starts_with("n_")) %>%
  glimpse()

# rates x year x age x race ####
# 4.1 Annual number of suicides and rates of suicide (suicides per 100,000 population), by race and ethnicity and age group, Coconino County, 2017-2021 (rates require n>20 and suppress when n<10)
# check what levels exist for race
# unique(mortality_df_suicide$d_race_code)

# totals
# count of death by suicide for coconino residents by year
table_total_suicide_x_year <- mortality_df_suicide %>%
  # filter(
  #   death_book_year %in% as.character(seq(2017, 2021, 1))
  # ) %>%
  group_by(death_book_year) %>%
  count() %>%
  ungroup() %>%
  mutate(d_race_code = "Total - All combined")

# view
table_total_suicide_x_year

# total count of suicide by race and year
table_total_suicide_x_year_race <- mortality_df_suicide %>%
  # filter(
  #   death_book_year %in% as.character(seq(2017, 2021, 1))
  # ) %>%
  group_by(
    death_book_year,
    d_race_code
  ) %>%
  count() %>%
  ungroup() %>%
  mutate(
    grouping_var = if_else(
      n >= 6,
      paste0(d_race_code, ":", death_book_year),
      paste0("Other:", death_book_year)
    )
  ) %>%
  group_by(grouping_var) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  separate(
    col = grouping_var,
    into = c("d_race_code", "death_book_year"),
    sep = ":"
  ) %>%
  bind_rows(
    table_total_suicide_x_year
  )

# view
table_total_suicide_x_year_race

# view as plot
table_total_suicide_x_year_race %>%
  filter(d_race_code != "Total - All combined") %>%
  ggplot(mapping = aes(
    x = death_book_year,
    y = n,
    group = d_race_code
  )) +
  geom_col(
    mapping = aes(
      fill = d_race_code
    ),
    alpha = 3 / 4
  ) +
  theme_linedraw()
#
# ggsave(
#   filename = "figures/plot.svg",
#   device = "svg"
# )

# rates
# population denominators
# matching age groups
# unique(mortality_df_suicide$age_group)

# which pins exist?
pin_list(board = project_pb)

# 2000 US Standard Population
us_std_pop <- pin_read(
  board = project_pb,
  name = "2000-us-standard-population"
)

# view
# us_std_pop

# AZDHS Population denominator for age and sex
pop_denom_age <- pin_read(
  board = project_pb,
  name = "azdhs-population-denominators-age-sex-race-2012-2021"
)

# view
# pop_denom_age

# transform azdhs population denominator to match 2000 us standard population age groups
pop_denom_age_coco <- pop_denom_age %>%
  # filter(
  #   year %in% as.character(seq(2017, 2021, 1))
  # ) %>%
  filter(area == "Coconino") %>%
  filter(sex == "Total") %>%
  filter(race_ethnicity == "All groups") %>%
  filter(age_group != "total") %>%
  mutate(
    age_group_us_std = case_when(
      age_group == "<1" ~ "0-4",
      age_group == "1-4" ~ "0-4",
      TRUE ~ as.character(age_group)
    ),
    age_group_us_std = str_c(age_group_us_std, " years", sep = "")
  ) %>%
  group_by(year, age_group_us_std) %>%
  summarise(estimate = sum(estimate)) %>%
  ungroup()

# view
# pop_denom_age_coco

# join azdhs population estimates with 2000 US Standard
pop_denom_us_std_pop <- full_join(
  x = us_std_pop,
  y = pop_denom_age_coco,
  by = c(
    "age_group" = "age_group_us_std"
  )
)

# view
# pop_denom_us_std_pop

# check frequency of age groups
# mortality_df_suicide %>%
#   tabyl(d_age_group_us_std)

# join totals with pop estimates and 2000 US Standard
table_suicide_n_x_age <- mortality_df_suicide %>%
  # filter(
  #   death_book_year %in% as.character(seq(2017, 2021, 1))
  # ) %>%
  tabyl(death_book_year, d_age_group_us_std) %>%
  # adorn_totals(c("row", "col")) %>%
  as_tibble() %>%
  pivot_longer(
    cols = c(-death_book_year),
    names_to = "age_group",
    values_to = "n"
  ) %>%
  # mutate(
  #   area = "Coconino",
  #   sex = "Total",
  #   race_ethnicity = "All groups"
  # ) %>%
  right_join(
    x = .,
    y = pop_denom_us_std_pop,
    by = c(
      "death_book_year" = "year",
      "age_group"
    )
  )

# view
table_suicide_n_x_age

# convert NA to 0 for calculation
table_suicide_n_x_age$n[is.na(table_suicide_n_x_age$n)] <- 0

# table of counts and age adjusted rates by year
table_suicide_age_adj_rates <- table_suicide_n_x_age %>%
  group_by(death_book_year) %>%
  mutate(pop_percent = standard_pop / sum(standard_pop)) %>%
  ungroup() %>%
  mutate(
    crude_rate = 100000 * (n / estimate),
    std_rate = pop_percent * crude_rate
  ) %>%
  group_by(death_book_year) %>%
  summarise(
    n = sum(n),
    population = sum(estimate),
    rate = round(100000 * (n / population), digits = 3),
    adj_rate = round(sum(std_rate), digits = 3)
  ) %>%
  ungroup()

# view
table_suicide_age_adj_rates

# view as plot
table_suicide_age_adj_rates %>%
  ggplot(mapping = aes(
    x = death_book_year,
    y = adj_rate
  )) +
  geom_col(alpha = 3 / 4) +
  theme_linedraw()

# levels of age group?
unique(table_suicide_n_x_age$age_group)

# table of counts and rates by year
table_suicide_age_adj_rates

# table of counts and rates for age
table_suicide_n_x_age %>%
  mutate(
    age_group = case_when(
      age_group == "0-4 years" ~ NA_character_,
      age_group == "5-9 years" ~ NA_character_,
      age_group == "10-14 years" ~ "10-24",
      age_group == "15-19 years" ~ "10-24",
      age_group == "20-24 years" ~ "10-24",
      age_group == "25-29 years" ~ "25-44",
      age_group == "30-34 years" ~ "25-44",
      age_group == "35-39 years" ~ "25-44",
      age_group == "40-44 years" ~ "25-44",
      age_group == "45-49 years" ~ "45-64",
      age_group == "50-54 years" ~ "45-64",
      age_group == "55-59 years" ~ "45-64",
      age_group == "60-64 years" ~ "45-64",
      TRUE ~ ">=65",
    )
  ) %>%
  group_by(death_book_year, age_group) %>%
  summarise(n = sum(n)) %>%
  pivot_wider(
    names_from = age_group,
    values_from = n
  ) %>%
  ungroup()

# table count and rate x year x age
table_total_rate_year_x_age <- table_suicide_n_x_age %>%
  mutate(
    age_group = case_when(
      age_group == "0-4 years" ~ NA_character_,
      age_group == "5-9 years" ~ NA_character_,
      age_group == "10-14 years" ~ "10-24",
      age_group == "15-19 years" ~ "10-24",
      age_group == "20-24 years" ~ "10-24",
      age_group == "25-29 years" ~ "25-44",
      age_group == "30-34 years" ~ "25-44",
      age_group == "35-39 years" ~ "25-44",
      age_group == "40-44 years" ~ "25-44",
      age_group == "45-49 years" ~ "45-64",
      age_group == "50-54 years" ~ "45-64",
      age_group == "55-59 years" ~ "45-64",
      age_group == "60-64 years" ~ "45-64",
      TRUE ~ ">=65",
    )
  ) %>%
  group_by(death_book_year, age_group) %>%
  summarise(
    n = sum(n),
    estimate = sum(estimate)
  ) %>%
  ungroup() %>%
  mutate(
    rate = case_when(
      n == 0 ~ 0,
      n < 6 ~ NA_integer_,
      n >= 6 ~ 100000 * (n / estimate)
    )
  ) %>%
  select(death_book_year, age_group, rate) %>%
  drop_na(age_group) %>%
  pivot_wider(
    names_from = age_group,
    values_from = rate
  ) %>%
  full_join(
    y = table_suicide_age_adj_rates
  ) %>%
  select(
    year = death_book_year,
    total = n,
    age_adj_rate = adj_rate,
    `10-24`,
    `25-44`,
    `45-64`,
    `>=65`
  )

# view
table_total_rate_year_x_age

# save to project pin board
table_total_rate_year_x_age %>%
  filter(
    year %in% as.character(seq(2017, 2021, 1))
  ) %>%
  pin_write(
    board = project_pb,
    x = .,
    name = "total-annual-number-and-rates-by-age",
    title = "Annual number of suicides and rates of suicide, by age, Coconino County, 2017-2021.",
    description = "Annual number of suicides and rates of suicide, by age, Coconino County, 2017-2021.",
    type = "rds",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      source = "AZDHS Vital Statistics"
    )
  )

# rates Coconino x AZ x USA ####
# 4.2 Coconino County suicide mortality trends between 2012 and 2021, age-adjusted to the 2000 US population
# read rates for USA & AZ
cdc_rates <- pin_read(
  board = project_pb,
  name = "cdc-wonder-age-adjusted-rates-suicide-2012-2021"
) %>%
  mutate(year = as.character(year))

# prepare table and save to pin board
table_suicide_age_adj_rates %>%
  select(
    year = death_book_year,
    rate = adj_rate
  ) %>%
  mutate(
    geography = "Coconino"
  ) %>%
  full_join(
    cdc_rates,
    by = c(
      "year",
      "rate" = "age_adjusted_rate",
      "geography"
    )
  ) %>%
  select(
    year,
    rate,
    geography
  ) %>%
  # ggplot(mapping = aes(
  #   x = year,
  #   y = rate,
  #   group = geography
  # )) +
  # geom_col(mapping = aes(
  #   fill = geography
  # ))
  pin_write(
    board = project_pb,
    name = "annual-age-adj-rates",
    type = "rds",
    title = "Annual age-adjusted rates, Coconino County, 2012-2021",
    description = "Annual age-adjusted rates, Coconino County, 2012-2021",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      source = "AZDHS Vital Statistics"
    )
  )

# 4.3 Percent change in annual suicide rate, Coconino County, 2012-2021
# percent change in rate ####
rate_change_12_21 <- table_suicide_age_adj_rates %>%
  filter(
    death_book_year %in% c("2012", "2021")
  ) %>%
  mutate(
    pct_change = (adj_rate - lag(adj_rate)) / lag(adj_rate) * 100
  ) %>%
  drop_na(pct_change) %>%
  mutate(
    year = "2012-2021",
    group = "several"
  )

rate_change_17_21 <- table_suicide_age_adj_rates %>%
  filter(
    death_book_year %in% c("2017", "2021")
  ) %>%
  mutate(
    pct_change = (adj_rate - lag(adj_rate)) / lag(adj_rate) * 100
  ) %>%
  drop_na(pct_change) %>%
  mutate(
    year = "2017-2021",
    group = "several"
  )

table_rate_pct_change <- table_suicide_age_adj_rates %>%
  mutate(
    pct_change = (adj_rate - lag(adj_rate)) / lag(adj_rate) * 100
  ) %>%
  mutate(
    year = lag(death_book_year)
  ) %>%
  mutate(
    year = str_c(
      year,
      death_book_year,
      sep = "-"
    ),
    group = "year-to-year"
  ) %>%
  full_join(
    y = rate_change_12_21
  ) %>%
  full_join(
    y = rate_change_17_21
  ) %>%
  select(
    year, adj_rate, pct_change, group
  )

# view
table_rate_pct_change

# save to pin
table_rate_pct_change %>%
  pin_write(
    board = project_pb,
    name = "annual-age-adj-rates-percent-change",
    type = "rds",
    title = "Annual age-adjusted rates, percent change, Coconino County, 2012-2021",
    description = "Annual age-adjusted rates, percent change, Coconino County, 2012-2021",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      source = "AZDHS Vital Statistics"
    )
  )

# 4.4 Selected demographic and descriptive characteristics of suicides, Coconino County, 2017-2021
# odds ratios ####

# Sex ####
table_x_sex <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  drop_na(d_suicide, d_sex) %>%
  mutate(
    sex = as.character(d_sex)
    # sex = if_else(
    # condition = (d_sex == "Male"),
    # true = 1, # male
    # false = 0 )# female
  ) %>%
  tabyl(sex, d_suicide)


table_x_sex
# fisher test where male = exposure and suicide = condition
#    sex    0   1 Total
# Female 3631  85  3716
#   Male 4834 283  5117
#  Total 8465 368  8833

fisher.test(table_x_sex)
chisq.test(table_x_sex)

# manual calculation of odds ratios
# where female = exposure and suicide = condition
odds_female <- matrix(
  data = c(85, 283, 3591, 4785),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_female)
chisq.test(odds_female)

odds_female <- (table_x_sex[1, 3] / table_x_sex[1, 2]) / (table_x_sex[2, 3] / table_x_sex[2, 2])

odds_male <- (table_x_sex[2, 3] / table_x_sex[2, 2]) / (table_x_sex[1, 3] / table_x_sex[1, 2])

# values taken from fisher.test and chisq.test above
odds_sex <- tibble(
  sex = c("Female", "Male"),
  OR = c(odds_female, odds_male),
  CI_lower = c(0.3090954, 1.945882),
  CI_upper = c(0.5139059, 3.235247),
  "Chi-square p-value" = c("<0.01", "<0.01")
)

# assemble data into desired table format
table_sex <- full_join(
  x = as_tibble(table_x_sex),
  y = odds_sex
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Sex"
  ) %>%
  select(
    Characteristic,
    "char_value" = sex,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  )

# view
table_sex

# Age ####
unique(mortality_df$d_age_group_5yr)

table_x_age <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  mutate(
    age_group = case_when(
      d_age_group_5yr == "0-4 years" ~ NA_character_,
      d_age_group_5yr == "5-9 years" ~ NA_character_,
      d_age_group_5yr == "10-14 years" ~ "10-24",
      d_age_group_5yr == "15-19 years" ~ "10-24",
      d_age_group_5yr == "20-24 years" ~ "10-24",
      d_age_group_5yr == "25-29 years" ~ "25-44",
      d_age_group_5yr == "30-34 years" ~ "25-44",
      d_age_group_5yr == "35-39 years" ~ "25-44",
      d_age_group_5yr == "40-44 years" ~ "25-44",
      d_age_group_5yr == "45-49 years" ~ "45-64",
      d_age_group_5yr == "50-54 years" ~ "45-64",
      d_age_group_5yr == "55-59 years" ~ "45-64",
      d_age_group_5yr == "60-64 years" ~ "45-64",
      TRUE ~ ">=65",
    )
  ) %>%
  drop_na(d_suicide, age_group) %>%
  tabyl(age_group, d_suicide)

# view
table_x_age %>%
  adorn_totals(where = c("row", "col"))
# age_group    0   1 Total
#      >=65 5190  52  5242
#     10-24  184  70   254
#     25-44  943 152  1095
#     45-64 2060  94  2154
#     Total 8377 368  8745

chisq.test(table_x_age)

# where age group 10-24 = exposure and suicide = condition
age_10_24 <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  mutate(
    age_group = case_when(
      d_age_group_5yr == "10-14 years" ~ "10-24",
      d_age_group_5yr == "15-19 years" ~ "10-24",
      d_age_group_5yr == "20-24 years" ~ "10-24",
      TRUE ~ "Unexposed",
    )
  ) %>%
  drop_na(d_suicide, age_group) %>%
  tabyl(age_group, d_suicide)

age_10_24 %>% adorn_totals(where = c("row", "col"))

fisher.test(age_10_24)
chisq.test(age_10_24)

odds_age_10_24 <- matrix(
  data = c(70, 298, 184, 8192),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_age_10_24)
chisq.test(odds_age_10_24)

# where age group 25-44 = exposure and suicide = condition
age_25_44 <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  mutate(
    age_group = case_when(
      d_age_group_5yr == "25-29 years" ~ "25-44",
      d_age_group_5yr == "30-34 years" ~ "25-44",
      d_age_group_5yr == "35-39 years" ~ "25-44",
      d_age_group_5yr == "40-44 years" ~ "25-44",
      TRUE ~ "Unexposed",
    )
  ) %>%
  drop_na(d_suicide, age_group) %>%
  tabyl(age_group, d_suicide)

age_25_44 %>% adorn_totals(where = c("row", "col"))

fisher.test(age_25_44)
chisq.test(age_25_44)

odds_age_25_44 <- matrix(
  data = c(152, 216, 943, 7433),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_age_25_44)
chisq.test(odds_age_25_44)

# where age group 45-64 = exposure and suicide = condition
age_45_64 <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  mutate(
    age_group = case_when(
      d_age_group_5yr == "45-49 years" ~ "45-64",
      d_age_group_5yr == "50-54 years" ~ "45-64",
      d_age_group_5yr == "55-59 years" ~ "45-64",
      d_age_group_5yr == "60-64 years" ~ "45-64",
      TRUE ~ "Unexposed",
    )
  ) %>%
  drop_na(d_suicide, age_group) %>%
  tabyl(age_group, d_suicide)

age_45_64

fisher.test(age_45_64)
chisq.test(age_45_64)

odds_age_45_64 <- matrix(
  data = c(94, 274, 2060, 6316),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_age_45_64)
chisq.test(odds_age_45_64)

# where age group >=65 = exposure and suicide = condition
age_65_up <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  mutate(
    age_group = case_when(
      calc_age >= 65 ~ ">=65",
      TRUE ~ "Unexposed",
    )
  ) %>%
  drop_na(d_suicide, age_group) %>%
  tabyl(age_group, d_suicide)

age_65_up

fisher.test(age_65_up)
chisq.test(age_65_up)

odds_age_65_up <- matrix(
  data = c(52, 316, 5189, 3187),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_age_65_up)
chisq.test(odds_age_65_up)

# values taken from fisher.test and chisq.test above
odds_age <- tibble(
  age_group = c("10-24", "25-44", "45-64", ">=65"),
  OR = c(10.45007, 5.545003, 1.051844, 0.1011123),
  CI_lower = c(7.634648, 4.426225, 0.8186961, 0.07364673),
  CI_upper = c(14.197199, 6.934012, 1.3418141, 0.13639859),
  "Chi-square p-value" = c("<0.01", "<0.01", "0.72", "<0.01")
)

# assemble data into desired table format
table_age <- full_join(
  x = as_tibble(table_x_age),
  y = odds_age
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Age"
  ) %>%
  select(
    Characteristic,
    "char_value" = age_group,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  ) %>%
  arrange(char_value)

# view
table_age

# race ####
unique(mortality_df$d_race_code)
tabyl(mortality_df$d_race_code)

mortality_df_x_race <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  drop_na(d_race_code, d_suicide) %>%
  mutate(
    d_race_white = if_else(
      condition = d_race_code == "White Non-Hispanic",
      true = 1,
      false = 0
    ),
    d_race_aian = if_else(
      condition = d_race_code == "American Indian and Alaska Native",
      true = 1,
      false = 0
    ),
    d_race_hispanic = if_else(
      condition = d_race_code == "Hispanic or Latino (any race)",
      true = 1,
      false = 0
    ),
    d_race_black = if_else(
      condition = d_race_code == "Black or African American",
      true = 1,
      false = 0
    ),
    d_race_asian = if_else(
      condition = d_race_code == "Asian or Native Hawaiian and Other Pacific Islander",
      true = 1,
      false = 0
    ),
    d_race_other = if_else(
      condition = d_race_code == "Other",
      true = 1,
      false = 0
    )
  )

table_x_race <- mortality_df_x_race %>%
  # filter(d_race_code != "Other") %>%
  mutate(d_race_code = as.character(d_race_code)) %>%
  tabyl(d_race_code, d_suicide)

table_x_race %>%
  adorn_totals(where = c("row", "col"))

mortality_df_x_race %>%
  select(
    starts_with("d_race"),
    d_suicide
  ) %>%
  pivot_longer(
    cols = -c(d_race_code, d_suicide),
    names_to = "race",
    values_to = "value"
  ) %>%
  select(-d_race_code) %>%
  filter(value == 1) %>%
  tabyl(race, d_suicide)

# where White Non-Hispanic = exposure and suicide = condition
race_white <- mortality_df_x_race %>%
  tabyl(d_race_white, d_suicide)

race_white %>% adorn_totals(where = c("row", "col"))

fisher.test(race_white)
chisq.test(race_white)

odds_race_white <- matrix(
  data = c(217, 151, 4320, 4056),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_race_white)
chisq.test(odds_race_white)

# where American Indian and Alaska Native = exposure and suicide = condition
race_aian <- mortality_df_x_race %>%
  tabyl(d_race_aian, d_suicide)

race_aian %>% adorn_totals(where = c("row", "col"))

fisher.test(race_aian)
chisq.test(race_aian)

odds_race_aian <- matrix(
  data = c(108, 260, 3271, 5105),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_race_aian)
chisq.test(odds_race_aian)

# where Hispanic or Latino (any race) = exposure and suicide = condition
race_hisp <- mortality_df_x_race %>%
  tabyl(d_race_hispanic, d_suicide)

race_hisp %>% adorn_totals(where = c("row", "col"))

fisher.test(race_hisp)
chisq.test(race_hisp)

odds_race_hisp <- matrix(
  data = c(34, 334, 601, 7775),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_race_hisp)
chisq.test(odds_race_hisp)

# where Black or African American = exposure and suicide = condition
race_black <- mortality_df_x_race %>%
  tabyl(d_race_black, d_suicide)

race_black %>% adorn_totals(where = c("row", "col"))

fisher.test(race_black)
chisq.test(race_black)

odds_race_black <- matrix(
  data = c(4, 364, 76, 8300),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_race_black)
chisq.test(odds_race_black)

# where Asian or Native Hawaiian and Other Pacific Islander = exposure and suicide = condition
race_asian <- mortality_df_x_race %>%
  tabyl(d_race_asian, d_suicide)

race_asian %>% adorn_totals(where = c("row", "col"))

fisher.test(race_asian)
chisq.test(race_asian)

odds_race_asian <- matrix(
  data = c(4, 364, 76, 8300),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_race_asian)
chisq.test(odds_race_asian)

# values taken from fisher.test and chisq.test above
odds_race <- tibble(
  d_race_code = c("White Non-Hispanic", "American Indian and Alaska Native", "Hispanic or Latino (any race)", "Black or African American", "Asian or Native Hawaiian and Other Pacific Islander"),
  OR = c(1.349214, 0.6483156, 1.316867, 1.200087, 1.200087),
  CI_lower = c(1.086104, 0.5108624, 0.8881682, 0.317100, 0.317100),
  CI_upper = c(1.679530, 0.8181866, 1.8982739, 3.227885, 3.227885),
  "Chi-square p-value" = c("<0.01", "<0.01", "0.16", "0.94", "0.94")
)


# assemble data into desired table format
table_race <- full_join(
  x = as_tibble(table_x_race),
  y = odds_race
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    `1` = case_when(
      `1` == 0 ~ 0,
      `1` < 6 ~ NA_integer_,
      `1` >= 6 ~ `1`
    ),
    pct = case_when(
      `1` == 0 ~ 0,
      `1` < 6 ~ NA_integer_,
      `1` >= 6 ~ pct
    )
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Race-Ethnicity"
  ) %>%
  select(
    Characteristic,
    "char_value" = d_race_code,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  ) %>%
  filter(char_value != "Other")

# view
table_race

# military service ####
mortality_df %>%
  select(contains("mili")) %>%
  glimpse()

table_x_military <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  drop_na(d_suicide, military_service) %>%
  mutate(
    d_military_service = if_else(
      condition = military_service == "yes",
      true = "Military service",
      false = "0"
    )
  ) %>%
  tabyl(d_military_service, d_suicide)


table_x_military

fisher.test(table_x_military)
chisq.test(table_x_military)

# manual calculation of odds ratios
# where military service = exposure and suicide = condition
odds_military <- matrix(
  data = c(38, 330, 1594, 6768),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_military)
chisq.test(odds_military)

# assemble data into desired table format
table_military <- tibble(
  as_tibble(table_x_military),
  OR = c(NA_integer_, 0.4889703),
  CI_lower = c(NA_integer_, 0.3383996),
  CI_upper = c(NA_integer_, 0.6888697),
  "Chi-square p-value" = c("NA", "<0.01")
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Ever served in military"
  ) %>%
  select(
    Characteristic,
    "char_value" = d_military_service,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  ) %>%
  filter(char_value == "Military service")

# view
table_military

# method ####
mortality_df %>%
  select(starts_with("d_")) %>%
  glimpse()

# which methods exist?
mortality_df %>%
  mutate(
    d_inj = if_else(
      condition = d_suicide == 1,
      true = fct_lump_min(
        f = d_injurydesc_code,
        min = 2
      ),
      false = NA_character_
    )
  ) %>%
  tabyl(d_inj)

mortality_x_method <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  drop_na(d_suicide) %>%
  mutate(
    d_method = case_when(
      str_detect(
        string = d_injurydesc_code,
        pattern = "firearm"
      ) ~ "Firearm",
      str_detect(
        string = d_injurydesc_code,
        pattern = "hanging"
      ) ~ "Hanging/Strangulation/Suffocation",
      str_detect(
        string = d_injurydesc_code,
        pattern = "hung "
      ) ~ "Hanging/Strangulation/Suffocation",
      str_detect(
        string = d_injurydesc_code,
        pattern = "intoxication"
      ) ~ "Poisoning",
      str_detect(
        string = d_injurydesc_code,
        pattern = "poison"
      ) ~ "Poisoning",
      TRUE ~ "Other"
    )
  )

table_x_method <- mortality_x_method %>%
  filter(d_method != "Other") %>%
  tabyl(d_method, d_suicide)

table_x_method %>%
  adorn_totals(where = c("row", "col"))

# manual calculation of odds ratios
# where firearm = exposure and suicide = condition
table_method_firearm <- mortality_x_method %>%
  mutate(
    d_method_firearm = if_else(
      condition = d_method == "Firearm",
      true = 1,
      false = 0
    )
  ) %>%
  tabyl(d_method_firearm, d_suicide)

table_method_firearm %>% adorn_totals(where = c("row", "col"))

fisher.test(table_method_firearm)

odds_method_firearm <- matrix(
  data = c(171, 197, 40, 8336),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_method_firearm)
chisq.test(odds_method_firearm)

# where hanging = exposure and suicide = condition
table_method_hanging <- mortality_x_method %>%
  mutate(
    d_method_hanging = if_else(
      condition = d_method == "Hanging/Strangulation/Suffocation",
      true = 1,
      false = 0
    )
  ) %>%
  tabyl(d_method_hanging, d_suicide)

table_method_hanging %>% adorn_totals(where = c("row", "col"))

fisher.test(table_method_hanging)

odds_method_hanging <- matrix(
  data = c(108, 260, 2, 8374),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_method_hanging)
chisq.test(odds_method_hanging)

# where poisoning = exposure and suicide = condition
table_method_poisoning <- mortality_x_method %>%
  mutate(
    d_method_poisoning = if_else(
      condition = d_method == "Poisoning",
      true = 1,
      false = 0
    )
  ) %>%
  tabyl(d_method_poisoning, d_suicide)

table_method_poisoning %>% adorn_totals(where = c("row", "col"))

fisher.test(table_method_poisoning)

odds_method_poisoning <- matrix(
  data = c(29, 339, 303, 8162),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_method_poisoning)
chisq.test(odds_method_poisoning)

# assemble data into desired table format
odds_method <- tibble(
  d_method = c("Firearm", "Hanging/Strangulation/Suffocation", "Poisoning"),
  OR = c(181.4543, 1719.275, 2.304166),
  CI_lower = c(124.2053, 471.9381, 1.493877),
  CI_upper = c(272.2572, 16384.0000, 3.438953),
  "Chi-square p-value" = c("<0.01", "<0.01", "<0.01")
)

table_method <- full_join(
  x = as_tibble(table_x_method),
  y = odds_method
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Method"
  ) %>%
  select(
    Characteristic,
    "char_value" = d_method,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  )

# view
table_method

# substance detected ####
mortality_df %>%
  select(starts_with("d_")) %>%
  glimpse()

mortality_x_substance <- mortality_df %>%
  filter(death_book_year %in% as.character(seq(2017, 2021, 1))) %>%
  drop_na(d_suicide)

mortality_x_substance %>%
  tabyl(d_substance_abuse)

table_x_substance <- mortality_x_substance %>%
  tabyl(d_substance_abuse, d_suicide)

table_x_substance %>%
  adorn_totals(where = c("row", "col"))

# manual calculation of odds ratios
# where alcohol = exposure and suicide = condition
table_substance_alcohol <- mortality_x_substance %>%
  tabyl(d_alcohol, d_suicide)

table_substance_alcohol %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_alcohol)

odds_substance_alcohol <- matrix(
  data = c(83, 285, 1106, 7270),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_alcohol)
chisq.test(odds_substance_alcohol)

# where amphetamines = exposure and suicide = condition
table_substance_amphetamines <- mortality_x_substance %>%
  tabyl(d_amphetamines, d_suicide)

table_substance_amphetamines %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_amphetamines)

odds_substance_amphetamines <- matrix(
  data = c(23, 345, 193, 8183),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_amphetamines)
chisq.test(odds_substance_amphetamines)

# where cannabis = exposure and suicide = condition
table_substance_cannabis <- mortality_x_substance %>%
  tabyl(d_cannabis, d_suicide)

table_substance_cannabis %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_cannabis)

odds_substance_cannabis <- matrix(
  data = c(15, 353, 40, 8336),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_cannabis)
chisq.test(odds_substance_cannabis)

# where cocaine = exposure and suicide = condition
table_substance_cocaine <- mortality_x_substance %>%
  tabyl(d_cocaine, d_suicide)

table_substance_cocaine %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_cocaine)

odds_substance_cocaine <- matrix(
  data = c(3, 365, 26, 8350),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_cocaine)
chisq.test(odds_substance_cocaine)

# where heroin = exposure and suicide = condition
table_substance_heroin <- mortality_x_substance %>%
  tabyl(d_heroin, d_suicide)

table_substance_heroin %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_heroin)

odds_substance_heroin <- matrix(
  data = c(1, 367, 24, 8352),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_heroin)
chisq.test(odds_substance_heroin)

# where opioids = exposure and suicide = condition
table_substance_opioids <- mortality_x_substance %>%
  tabyl(d_opioids, d_suicide)

table_substance_opioids %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_opioids)

odds_substance_opioids <- matrix(
  data = c(15, 353, 165, 8299),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_opioids)
chisq.test(odds_substance_opioids)

# where opioids_rx = exposure and suicide = condition
table_substance_opioids_rx <- mortality_x_substance %>%
  tabyl(d_opioids_rx, d_suicide)

table_substance_opioids_rx %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_opioids_rx)

odds_substance_opioids_rx <- matrix(
  data = c(15, 353, 160, 8216),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_opioids_rx)
chisq.test(odds_substance_opioids_rx)

# where sedatives = exposure and suicide = condition
table_substance_sedatives <- mortality_x_substance %>%
  tabyl(d_sedatives, d_suicide)

table_substance_sedatives %>% adorn_totals(where = c("row", "col"))

fisher.test(table_substance_sedatives)

odds_substance_sedatives <- matrix(
  data = c(10, 358, 32, 8344),
  nrow = 2,
  ncol = 2
)

fisher.test(odds_substance_sedatives)
chisq.test(odds_substance_sedatives)

# assemble data into desired table format
odds_substance <- tibble(
  substance = c("Alcohol", "Amphetamines", "Cannabis", "Cocaine", "Heroin", "Opioids", "Rx Opioids", "Sedatives"),
  OR = c(1.914223, 2.826057, 8.850134, 2.639182, 0.9482343, 2.136994, 2.181645, 7.279416),
  CI_lower = c(1.468354, 1.726284, 4.496657, 0.5089466, 0.02299657, 1.156945, 1.180393, 3.166316),
  CI_upper = c(2.473875, 4.436045, 16.563251, 8.6711673, 5.84798455, 3.677369, 3.757673, 15.324434),
  "Chi-square p-value" = c("<0.01", "<0.01", "<0.01", "0.24", "1", "0.01", "<0.01", "<0.01")
)

table_substance <- tibble(
  substance = c("Alcohol", "Amphetamines", "Cannabis", "Cocaine", "Heroin", "Opioids", "Rx Opioids", "Sedatives"),
  `0` = c(285, 193, 40, 26, 24, 165, 160, 32),
  `1` = c(83, 23, 15, 3, 1, 15, 15, 10)
)

table_substance <- full_join(
  x = table_substance,
  y = odds_substance
) %>%
  mutate(
    pct = (`1` / sum(`1`) * 100)
  ) %>%
  mutate(
    `1` = case_when(
      `1` == 0 ~ 0,
      `1` < 6 ~ NA_integer_,
      `1` >= 6 ~ `1`
    ),
    pct = case_when(
      `1` == 0 ~ 0,
      `1` < 6 ~ NA_integer_,
      `1` >= 6 ~ pct
    )
  ) %>%
  mutate(
    "Total(%)" = str_c(
      `1`,
      " (",
      round(pct, digits = 1),
      ")"
    ),
    "OR(95% CI)" = str_c(
      round(OR, digits = 1),
      " (",
      round(CI_lower, digits = 1),
      "-",
      round(CI_upper, digits = 1),
      ")"
    ),
    "Characteristic" = "Substance detected"
  ) %>%
  select(
    Characteristic,
    "char_value" = substance,
    `Total(%)`,
    `Chi-square p-value`,
    `OR(95% CI)`
  )

# view
table_substance

# join all odds ratio tables
characteristics_stats <- bind_rows(
  table_sex,
  table_age,
  table_race,
  table_military,
  table_method,
  table_substance
)

# example table
characteristics_stats %>%
  group_by(Characteristic) %>%
  gt::gt()

characteristics_stats %>%
  pin_write(
    board = project_pb,
    x = .,
    name = "descriptive-characteristics-statistics",
    title = "Selected demographic and descriptive characteristics of suicides, Coconino County, 2017-2021.",
    description = "Selected demographic and descriptive characteristics of suicides among persons aged >= 10 years, Coconino County, 2017-2021.",
    type = "rds",
    metadata = list(
      user = "rherrera",
      owner = "Coconino HHS",
      department = "Epidemiology",
      source = "AZDHS Vital Statistics"
    )
  )

# 4.5 Geography Geospatial

# injury location #### 
library(tigris)
options(tigris_use_cache = TRUE)

library(sf)

library(tmap)


library(basemaps)
library(magick)
library(raster)
library(rgdal)

options(tigris_use_cache = TRUE)

coco_county_spatial <- counties(state = "AZ") %>%
  clean_names() %>%
  filter(name == "Coconino")

plot(coco_county_spatial)

coco_county_muni <- places(state = "AZ") %>%
  clean_names() %>%
  filter(name %in% c("Flagstaff", "Williams", "Fredonia", "Page", "Sedona"))

coco_bbox <- st_bbox(coco_county_spatial)

tmap_mode("plot")

tm_shape(
  shp = coco_county_spatial,
  bbox = coco_bbox
) +
  tm_basemap("Stamen.Watercolor")
  

mortality_df_suicide %>%
  sample_n(size = 10) %>%
  select(contains("n_")) %>%
  glimpse()

unique(mortality_df_suicide$injury_add_lattitude)

data(ext)

image_browse(basemap_magick(ext))

bm <- basemap_magick(
  ext = st_bbox(coco_county_spatial),
  map_service = "osm_stamen",
  map_type = "watercolor"
)

dem_01 <- image_read(
  path = "../../Downloads/USGS_1_n36w112_20230418.tif"
)

plot(dem_01)

print(dem_01)
image_browse(dem_01)

GDALinfo("../../Downloads/USGS_1_n36w112_20230418.tif")

image_write(
  image = dem_01,
  path = "../../Downloads/tif-to-png.png",
  format = "png"
)

print(bm)
image_browse(ext)

ggplot() +
  basemap_gglayer(st_bbox(coco_county_spatial)) +
  geom_sf(
    data = coco_county_spatial,
    color = "black",
    linewidth = 1
  ) +
  geom_sf_label(
    data = coco_county_muni,
    mapping = aes(label = name)
  )


# 4.7 Military or veteran status



# 4.8 Access to Lethal Means



# 4.9 Suicide by Month, 2017-2021


# 4.10 Location type, 2017-2021


# 4.11 Marital status , 2017-2021


# 4.12 Highest level of Education level, 2017-2021


# 4.13 Place of birth , 2017-2021


# 4.14 Toxicology Results, 2017-2021


# 4.15 Top leading causes of death and years of potential life lost (YPLL) before age 75 among Coconino County residents, 2021


# 4.16 Age-specific suicide mortality rates among youth aged 10-24 years, Coconino County, 2017-2021


# 4.17 By occupation, 2017-2021






# Location type
