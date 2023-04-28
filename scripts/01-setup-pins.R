# package libraries ####
library(here)
library(tidyverse)
library(pins)



# mortality data pin board ####
mortality_pb <- board_folder(
  path = "S:/HIPAA Compliance/SAS Files/Coconino Deaths/mortality-data"
)

mortality_pb %>%
  pin_list()

# hospital discharge data pin board ####
hdd_pb <- board_folder(
  path = "S:/HIPAA Compliance/Hospital Discharge Data/r-pin-board-rds-files/"
)

hdd_pb %>%
  pin_list()

# project pin board ####
project_pb <- board_folder(
  path = "data-tidy"
)

project_pb %>%
  pin_list()
