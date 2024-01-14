rm(list = ls())

library(tidyverse)
library(leaflet)

########################################################################
# Prepare data
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))
block.demographics <- read_csv("census-blocks/wi-blocks-simple.csv",
                               col_types = "cccccccccnnnnnnnnnnnnnnnnnn")

blocks.with.mcds <- block.assignments |>
  inner_join(block.demographics) |>
  select(GEOID, ends_with("wsa"), ends_with("wss"),
         CNTY_NAME, muni_fips, WARD_FIPS, corrected_WARD_FIPS, MCD_NAME, CTV, pop) |>
  pivot_longer(cols = c(ends_with("wsa"), ends_with("wss")),
               names_to = "plan", values_to = "district") |>
  filter(district != "ZZZ")

########################################################################
# county splits
district.county.intersections <- blocks.with.mcds |>
  group_by(CNTY_NAME, plan, district) |>
  summarise(blocks = n(),
            pop = sum(pop), .groups = "drop")

# identify municipalities divided into multiple districts
#   that themselves straddle municipal lines
split.counties <- district.county.intersections |>
  # number of counties which this district at least partially overlaps with
  group_by(plan, district) |>
  mutate(counties_in_district = n_distinct(CNTY_NAME)) |>
  ungroup() |>
  # remove districts that fall entirely within a single county
  filter(counties_in_district > 1) |>
  # number of districts which this county at least partially overlaps with
  group_by(plan, CNTY_NAME) |>
  mutate(districts_in_county = n_distinct(district)) |>
  ungroup() |>
  filter(districts_in_county > 1)

split.counties.count <- split.counties |>
  group_by(plan) |>
  summarise(split_counties = n_distinct(CNTY_NAME))

########################################################################
# municipality splits
district.muni.intersections <- blocks.with.mcds |>
  filter(muni_fips != "00000") |>
  group_by(muni_fips, plan, district) |>
  summarise(blocks = n(),
            pop = sum(pop), .groups = "drop")

# identify municipalities divided into multiple districts
#   that themselves straddle municipal lines
split.municipalities <- district.muni.intersections |>
  # number of municipalities which this district at least partially overlaps with
  group_by(plan, district) |>
  mutate(munis_in_district = n_distinct(muni_fips)) |>
  ungroup() |>
  # remove districts that fall entirely within a single municipality
  filter(munis_in_district > 1) |>
  # number of districts which this municipality at least partially overlaps with
  group_by(plan, muni_fips) |>
  mutate(districts_in_muni = n_distinct(district)) |>
  ungroup() |>
  filter(districts_in_muni > 1)

split.municipalities.count <- split.municipalities |>
  group_by(plan) |>
  summarise(split_municipalities = n_distinct(muni_fips))

########################################################################
# ward splits
# list of blocks w/corrected wards
#   from Joint Stipulation, "parties agree that detaching any of the 216 ward 
#     fragments identified in Appendix A from the rest of the ward to which it 
#     is assigned in the August 2021 Redistricting Dataset will not count as a
#     ward split when evaluating a proposed remedial map
blocks.appendix.a <- blocks.with.mcds |>
  filter(WARD_FIPS != corrected_WARD_FIPS)

district.wards.int <- blocks.with.mcds |>
  # remove appendix A blocks
  filter(! WARD_FIPS %in% blocks.appendix.a$WARD_FIPS) |>
  group_by(WARD_FIPS, plan) |>
  summarise(districts = n_distinct(district), .groups = "drop")

# identify wards divided into multiple districts
split.wards.count <- district.wards.int |>
  group_by(plan) |>
  summarise(split_wards = sum(districts > 1))

########################################################################
# combine data
all.splits <- full_join(split.municipalities.count, split.counties.count) |>
  full_join(split.wards.count)
write_csv(all.splits, "analysis-r/tables/plan-muni-and-county-splits.csv")
