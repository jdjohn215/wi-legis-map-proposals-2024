library(tidyverse)

# This script calculates the number of counties and municipalities split into
#   multiple legislative districts

########################################################################
# Prepare data
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))
block.demographics <- read_csv("census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID))

blocks.with.mcds <- block.assignments |>
  inner_join(block.demographics) |>
  select(GEOID, ends_with("wsa"), ends_with("wss"),
         CNTY_NAME, muni_fips, MCD_NAME, CTV, pop) |>
  pivot_longer(cols = c(ends_with("wsa"), ends_with("wss")),
               names_to = "plan", values_to = "district") |>
  filter(district != "ZZZ")

########################################################################
# Find county/district intersections
district.county.intersections <- blocks.with.mcds |>
  group_by(CNTY_NAME, plan, district) |>
  summarise(pop = sum(pop)) |>
  group_by(CNTY_NAME, plan) |>
  mutate(pct_of_cnty = (pop/sum(pop))*100) |>
  group_by(plan, district) |>
  mutate(pct_of_district = (pop/sum(pop))*100) |>
  ungroup()

# identify counties divided into multiple districts
#   that themselves straddle county lines
split.counties <- district.county.intersections |>
  filter(pct_of_district < 100) |>
  group_by(plan, CNTY_NAME) |>
  filter(n_distinct(district) > 1) |>
  group_by(plan) |>
  summarise(split_counties = n_distinct(CNTY_NAME))

########################################################################
# Find municipality/district intersections
district.muni.intersections <- blocks.with.mcds |>
  filter(muni_fips != "00000") |>
  group_by(muni_fips, plan, district) |>
  summarise(pop = sum(pop)) |>
  group_by(muni_fips, plan) |>
  mutate(pct_of_muni = (pop/sum(pop))*100) |>
  group_by(plan, district) |>
  mutate(pct_of_district = (pop/sum(pop))*100) |>
  ungroup()

# identify municipalities divided into multiple districts
#   that themselves straddle municipal lines
split.municipalities <- district.muni.intersections |>
  filter(pct_of_district < 100) |>
  group_by(muni_fips, plan) |>
  filter(n_distinct(district) > 1) |>
  group_by(plan) |>
  summarise(split_municipalities = n_distinct(muni_fips))

########################################################################
# combine data
all.splits <- full_join(split.municipalities, split.counties)
write_csv(all.splits, "analysis-r/tables/plan-muni-and-county-splits.csv")


