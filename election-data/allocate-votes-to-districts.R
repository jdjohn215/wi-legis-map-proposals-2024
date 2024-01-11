library(tidyverse)
library(sf)

# This script allocates votes from the 2022 general election into proposed legislative districts

################################################################################
# setup

# crosswalk to allocate reporting units to blocks
blocks.to.rep.units <- read_csv("election-data/blocks-to-reporting-units-allocation-factors.csv",
                                col_types = "cccn")

# block assignments to each redistricting plan
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c")) |>
  pivot_longer(cols = -GEOID, names_to = "plan", values_to = "district")

# the actual reporting unit election results
election.results <- st_read("election-data/ReportingUnitPolygons.geojson") |>
  st_drop_geometry()
#   keep results for governor, US senator, attorney general, and state treasurer
election.results.long <- election.results |>
  select(CNTY_NAME, rep_unit, contains("GOV"), contains("USS"), contains("WAG"), contains("WST")) |>
  pivot_longer(cols = c(contains("22")), names_to = "race", values_to = "votes")

################################################################################
# allocate votes

votes.by.district <- block.assignments |>
  inner_join(blocks.to.rep.units) |>
  group_by(plan, district, CNTY_NAME, rep_unit) |>
  summarise(prop_of_rep_unit = sum(prop_of_rep_unit), .groups = "drop") |>
  inner_join(election.results.long, relationship = "many-to-many") |>
  mutate(adj_votes = votes * prop_of_rep_unit) |>
  group_by(plan, district, race) |>
  summarise(votes = sum(adj_votes), .groups = "drop")

# confirm that vote totals match
#   each race
inner_join(
  votes.by.district |>
    group_by(race, plan) |>
    summarise(allocated_total = sum(votes)),
  election.results.long |>
    group_by(race) |>
    summarise(original_total = sum(votes))
) |>
  mutate(match = original_total == allocated_total) |>
  group_by(match) |>
  summarise(count = n())

# results in the actual districts
inner_join(
  election.results |>
    group_by(assembly_district) |>
    summarise(original_total = sum(GOVTOT22)),
  votes.by.district |>
    filter(plan == "legis_wsa",
           race == "GOVTOT22") |>
    mutate(district = as.numeric(district)) |>
    select(assembly_district = district, allocated_total = votes)
) |>
  mutate(match = original_total == allocated_total) |>
  group_by(match) |>
  summarise(count = n())

################################################################################
# setup LTSB 2012-2020 votes
# crosswalk between census blocks and LTSB wards 2020
#   only for blocks with VAP
blocks.to.wards <- read_csv("election-data/blocks-to-wards2020-allocation-factors.csv",
                            col_types = "ccn")

# orig LTSB
ltsb.2020 <- read_csv("election-data/2012-2020_Election_Data_with_2020_Wards.csv")

# reformat
ltsb.2020.long <- ltsb.2020 |>
  select(-c(1,3:34)) |>
  rename(ward_GEOID = GEOID) |>
  pivot_longer(cols = -ward_GEOID, values_to = "votes", names_to = "race")

# block assignments to each redistricting plan
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c")) |>
  pivot_longer(cols = -GEOID, names_to = "plan", values_to = "district")

ltsb.votes.by.district <- block.assignments |>
  inner_join(blocks.to.wards, by = c("GEOID" = "block_GEOID")) |>
  group_by(plan, district, ward_GEOID) |>
  summarise(prop_of_ward = sum(prop_of_ward), .groups = "drop") |>
  inner_join(ltsb.2020.long, relationship = "many-to-many") |>
  mutate(adj_votes = votes * prop_of_ward) |>
  group_by(plan, district, race) |>
  summarise(votes = sum(adj_votes, na.rm = T), .groups = "drop")

# confirm that vote totals match
#   each race
inner_join(
  ltsb.votes.by.district |>
    group_by(race, plan) |>
    summarise(allocated_total = sum(votes)),
  ltsb.2020.long |>
    group_by(race) |>
    summarise(original_total = sum(votes, na.rm = T))
) |>
  mutate(match = round(original_total) == round(allocated_total)) |>
  group_by(match) |>
  summarise(count = n())


################################################################################
# combine and save output
votes.by.district.all <- votes.by.district |>
  bind_rows(ltsb.votes.by.district) |>
  filter(str_sub(race, 4, -3) %in% c("DEM", "REP", "TOT"),
         str_sub(race, 1, 3) %in% c("GOV", "USS", "WAG", "WST"))

write_csv(votes.by.district.all, "election-data/votes-in-proposed-districts_2012-22.csv")
