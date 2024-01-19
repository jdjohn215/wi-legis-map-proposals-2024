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
# LTSB results disaggregated to blocks
#   See election-data/2012-2020/disaggregate-ltsb-to-2020-blocks.R for details
blocks.disaggregated.votes <- read_csv("election-data/2012-2020/ltsb-disaggregated-to-2020-blocks_2012-20.csv.gz",
                                       col_types = "ccn")

ltsb.votes.by.district <- blocks.disaggregated.votes |>
  left_join(block.assignments, by = c("block_2020" = "GEOID"), relationship = "many-to-many") |>
  group_by(plan, district, race = name) |>
  summarise(votes = sum(disaggregated_votes))

################################################################################
# combine and save output
votes.by.district.all <- votes.by.district |>
  bind_rows(ltsb.votes.by.district) |>
  filter(str_sub(race, 4, -3) %in% c("DEM", "REP", "TOT"),
         str_sub(race, 1, 3) %in% c("GOV", "USS", "WAG", "WST", "PRE"))

write_csv(votes.by.district.all, "election-data/votes-in-proposed-districts_2012-22.csv")
