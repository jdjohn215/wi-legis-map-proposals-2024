rm(list = ls())

library(tidyverse)
library(sf)
library(tidycensus)
options(scipen = 999)

# This script interpolates block level VAP from 2010 to 2020
#   It disaggregates 2010 blocks into 2020 blocks using land area overlap
#   defined by the 2010-2020 official census bureau block relationship file

################################################################################
# blocks 2010 to blocks 2020 relationship file
#   https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html
blocks.10.to.20 <- read_delim("election-data/2012-2020/tab2010_tab2020_st55_wi.txt") |>
  # just keep Wisconsin
  filter(STATE_2010 == "55",
         STATE_2020 == "55") |>
  mutate(block_2010 = paste0(STATE_2010, COUNTY_2010, TRACT_2010, BLK_2010),
         block_2020 = paste0(STATE_2020, COUNTY_2020, TRACT_2020, BLK_2020)) |>
  select(block_2010, block_2020, AREALAND_2010, AREALAND_2020, AREALAND_INT) |>
  # make sure to drop any land from outside WI from the total
  group_by(block_2010) |>
  mutate(AREALAND_2010 = sum(AREALAND_INT)) |>
  ungroup() |>
  group_by(block_2020) |>
  mutate(AREALAND_2020 = sum(AREALAND_INT)) |>
  ungroup() |>
  mutate(prop_of_2010 = AREALAND_INT/AREALAND_2010,
         prop_of_2020 = AREALAND_INT/AREALAND_2020,
         prop_of_2010 = if_else(AREALAND_INT == 0, 0, prop_of_2010),
         prop_of_2020 = if_else(AREALAND_INT == 0, 0, prop_of_2020))

################################################################################
# fetch block VAP
# 2010
wi.block.2010.vap <- get_decennial("block", variables = "P003001", state = "WI",
                                   year = 2010, sumfile = "pl") |>
  select(block_2010 = GEOID, vap_2010 = value)
# 2020
wi.block.2020.vap <- get_decennial("block", variables = "P3_001N", state = "WI",
                                   year = 2020, sumfile = "pl") |>
  select(block_2020 = GEOID, vap_2020 = value)

################################################################################
# disaggregate using relationship file
blocks.2020.integ <- blocks.10.to.20 |>
  full_join(wi.block.2010.vap) |>
  full_join(wi.block.2020.vap) |>
  mutate(vap_2010 = if_else(is.na(vap_2010), 0, vap_2010),
         vap_2020 = if_else(is.na(vap_2020), 0, vap_2020),
         prop_of_2010 = if_else(vap_2010 == 0 & is.na(prop_of_2010), 1, prop_of_2010)) |>
  select(block_2010, block_2020, vap_2010, vap_2020, prop_of_2010, prop_of_2020) |>
  mutate(vap_2010_in_2020 = vap_2010 * prop_of_2010) |>
  group_by(block_2020, vap_2020) |>
  summarise(vap_2010 = sum(sum(vap_2010_in_2020)), .groups = "drop") |>
  filter(block_2020 %in% wi.block.2020.vap$block_2020)

# confirm that VAP totals match for each year
sum(blocks.2020.integ$vap_2010) == sum(wi.block.2010.vap$vap_2010)
sum(blocks.2020.integ$vap_2020) == sum(wi.block.2020.vap$vap_2020)

################################################################################
# interpolate VAP between 2010 and 2020
#   simple linear interpolation
block.vap.interpolated <- blocks.2020.integ |>
  mutate(vap_diff = vap_2020 - vap_2010,
         vap_2012 = vap_2010 + (vap_diff*0.2),
         vap_2014 = vap_2010 + (vap_diff*0.4),
         vap_2016 = vap_2010 + (vap_diff*0.6),
         vap_2018 = vap_2010 + (vap_diff*0.8)) |>
  select(block_2020, vap_2010, vap_2012, vap_2014, vap_2016, vap_2018, vap_2020)

################################################################################
# save output
write_csv(block.vap.interpolated, "census-blocks/blocks2020-with-interpolated-vap_2010-2020.csv")
