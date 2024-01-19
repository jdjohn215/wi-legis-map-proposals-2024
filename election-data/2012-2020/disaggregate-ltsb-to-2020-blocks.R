rm(list = ls())

library(tidyverse)
library(sf)

################################################################################
# setup
#   this file includes VAP for each 2020 census block, 
#     aggregated from 2010 based on land area overlap & interpolated for the
#     intervening years.
block.vap <- read_csv("census-blocks/blocks2020-with-interpolated-vap_2010-2020.csv",
                      col_types = "cnnnnnn")
block.shp <- st_read("census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson") |>
  rename(block_2020 = GEOID)
block.centroids <- block.shp |> mutate(geometry = st_centroid(geometry))

###############################################################################
# 2020
# LTSB disaggregated election returns in 2020 wards
ltsb.2020 <- st_read("election-data/2012-2020/2012-2020_Election_Data_with_2020_Wards/2012-2020_Election_Data_with_2020_Wards.shp") |>
  rename(ward_2020 = GEOID)

wards.2020 <- ltsb.2020 |>
  select(ward_2020) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_make_valid()

wards.2020.to.blocks <- st_join(block.centroids, wards.2020) |>
  st_drop_geometry()

# wards that failed to be assigned any blocks
ward.join.2020.v2 <- ltsb.2020 |> 
  filter(! ward_2020 %in% wards.2020.to.blocks$ward_2020) |>
  select(ward_2020, PRETOT20) |>
  filter(PRETOT20 > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_join(block.shp) |>
  st_drop_geometry()

wards.2020.to.blocks.v2 <- wards.2020.to.blocks |>
  filter(! block_2020 %in% ward.join.2020.v2$block_2020) |>
  bind_rows(ward.join.2020.v2) |>
  select(ward_2020, block_2020)

###############################################################################
# 2018
# LTSB disaggregated election returns in 2018 wards
ltsb.2018 <- st_read("election-data/2012-2020/WI_20122020_Election_Data_Wards_2018/20122020_Election_Data_with_2018_Wards.shp")

wards.2018 <- ltsb.2018 |>
  select(ward_2018 = WARD_FIPS) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_make_valid()

wards.2018.to.blocks <- st_join(block.centroids, wards.2018) |>
  st_drop_geometry()

# wards that failed to be assigned any blocks
ward.join.2018.v2 <- ltsb.2018 |> 
  rename(ward_2018 = WARD_FIPS) |>
  filter(! ward_2018 %in% wards.2018.to.blocks$ward_2018) |>
  select(ward_2018, GOVTOT18) |>
  filter(GOVTOT18 > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_join(block.shp) |>
  st_drop_geometry()

wards.2018.to.blocks.v2 <- wards.2018.to.blocks |>
  filter(! block_2020 %in% ward.join.2018.v2$block_2020) |>
  bind_rows(ward.join.2018.v2) |>
  select(ward_2018, block_2020)

###############################################################################
# 2017
# LTSB disaggregated election returns in 2017 wards (because I can't find a 2016 file)
ltsb.2017 <- st_read("election-data/2012-2020/WI_20122020_Election_Data_Wards_2017/WI_20122020_Election Data_Wards_2017.shp")

wards.2017 <- ltsb.2017 |>
  select(ward_2017 = GEOID) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_make_valid()

wards.2017.to.blocks <- st_join(block.centroids, wards.2017) |>
  st_drop_geometry()


# wards that failed to be assigned any blocks
ward.join.2017.v2 <- ltsb.2017 |> 
  rename(ward_2017 = GEOID) |>
  filter(! ward_2017 %in% wards.2017.to.blocks$ward_2017) |>
  select(ward_2017, PRETOT16) |>
  filter(PRETOT16 > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_join(block.shp) |>
  st_drop_geometry()

wards.2017.to.blocks.v2 <- wards.2017.to.blocks |>
  filter(! block_2020 %in% ward.join.2017.v2$block_2020) |>
  bind_rows(ward.join.2017.v2) |>
  select(ward_2017, block_2020)

###############################################################################
# 2014
# LTSB disaggregated election returns in 2014 wards (because I can't find a 2016 file)
ltsb.2014 <- st_read("election-data/2012-2020/WI_ElectionData_2014/WI_ElectionData_2014.shp")

wards.2014 <- ltsb.2014 |>
  select(ward_2014 = WARD_FIPS) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_make_valid()

wards.2014.to.blocks <- st_join(block.centroids, wards.2014) |>
  st_drop_geometry()

# wards that failed to be assigned any blocks
ward.join.2014.v2 <- ltsb.2014 |> 
  rename(ward_2014 = WARD_FIPS) |>
  filter(! ward_2014 %in% wards.2014.to.blocks$ward_2014) |>
  select(ward_2014, GOVTOT14) |>
  filter(GOVTOT14 > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_join(block.shp) |>
  st_drop_geometry()

wards.2014.to.blocks.v2 <- wards.2014.to.blocks |>
  filter(! block_2020 %in% ward.join.2014.v2$block_2020) |>
  bind_rows(ward.join.2014.v2) |>
  select(ward_2014, block_2020)

###############################################################################
# 2011
# LTSB disaggregated election returns in 2011 wards (because I can't find a 2012 file)
ltsb.2011 <- st_read("election-data/2012-2020/WI_20122020_Election_Data_Wards_2011/WI_20122020_Election_Data_Wards_2011.shp")

wards.2011 <- ltsb.2011 |>
  select(ward_2011 = WARD_FIPS) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_make_valid()

wards.2011.to.blocks <- st_join(block.centroids, wards.2011) |>
  st_drop_geometry()

# wards that failed to be assigned any blocks
ward.join.2011.v2 <- ltsb.2011 |> 
  rename(ward_2011 = WARD_FIPS) |>
  filter(! ward_2011 %in% wards.2011.to.blocks$ward_2011) |>
  select(ward_2011, PRETOT12, GOVTOT12) |>
  filter(PRETOT12 > 0 | GOVTOT12 > 0) |>
  mutate(geometry = st_centroid(geometry)) |>
  st_transform(crs = st_crs(block.shp)) |>
  st_join(block.shp) |>
  st_drop_geometry()

wards.2011.to.blocks.v2 <- wards.2011.to.blocks |>
  filter(! block_2020 %in% ward.join.2011.v2$block_2020) |>
  bind_rows(ward.join.2011.v2) |>
  select(ward_2011, block_2020)

###############################################################################
# Disaggregate wards to blocks
#   use most proximate ward shapefile and interpolated vap for the given year

# 2020
wards.to.blocks.2020 <- ltsb.2020 |>
  select(ward_2020, ends_with("20")) |>
  st_drop_geometry() |>
  pivot_longer(cols = -ward_2020, values_to = "votes") |>
  filter(votes > 0) |>
  inner_join(wards.2020.to.blocks.v2, relationship = "many-to-many") |>
  inner_join(block.vap |>
               select(block_2020, vap_2020)) |>
  group_by(ward_2020, name) |>
  mutate(ward_vap = sum(vap_2020),
         block_prop_of_ward = vap_2020/ward_vap,
         block_prop_of_ward = if_else(is.na(block_prop_of_ward), 0, block_prop_of_ward),
         # if any of a ward remains unallocated, add the remaining allocation to each
         #    constitutent block
         unallocated = 1 - sum(block_prop_of_ward, na.rm = T),
         block_prop_of_ward = block_prop_of_ward + unallocated/n(),
         disaggregated_votes = votes*block_prop_of_ward) |>
  ungroup()
sum(wards.to.blocks.2020$disaggregated_votes[wards.to.blocks.2020$name == "PRETOT20"], na.rm = T) == sum(ltsb.2020$PRETOT20, na.rm = T)

# 2018
wards.to.blocks.2018 <- ltsb.2018 |>
  # keep 2014 races as well because the 2014 vote totals are WRONG in the 2011, 2014, and 2017 files
  #   see: https://elections.wi.gov/sites/default/files/legacy/11.4.14%2520Summary%2520Results-all%2520offices.pdf
  select(ward_2018 = WARD_FIPS, ends_with("18"), ends_with("14")) |>
  select(-c(WHITE18, BLACK18, HISPANIC18, ASIAN18, AMINDIAN18, PISLAND18, OTHER18, OTHERMLT18, PERSONS18)) |>
  st_drop_geometry() |>
  pivot_longer(cols = -ward_2018, values_to = "votes") |>
  filter(votes > 0) |>
  inner_join(wards.2018.to.blocks.v2, relationship = "many-to-many") |>
  inner_join(block.vap |>
               select(block_2020, vap_2018)) |>
  group_by(ward_2018, name) |>
  mutate(ward_vap = sum(vap_2018),
         block_prop_of_ward = vap_2018/ward_vap,
         block_prop_of_ward = if_else(is.na(block_prop_of_ward), 0, block_prop_of_ward),
         # if any of a ward remains unallocated, add the remaining allocation to each
         #    constitutent block
         unallocated = 1 - sum(block_prop_of_ward, na.rm = T),
         block_prop_of_ward = block_prop_of_ward + unallocated/n(),
         disaggregated_votes = votes*block_prop_of_ward) |>
  ungroup()
sum(wards.to.blocks.2018$disaggregated_votes[wards.to.blocks.2018$name == "GOVTOT18"], na.rm = T) == sum(ltsb.2018$GOVTOT18, na.rm = T)
sum(wards.to.blocks.2018$disaggregated_votes[wards.to.blocks.2018$name == "GOVTOT14"], na.rm = T) == sum(ltsb.2018$GOVTOT14, na.rm = T)

# 2016 (using 2017 wards)
wards.to.blocks.2016 <- ltsb.2017 |>
  select(ward_2017 = GEOID, ends_with("16")) |>
  st_drop_geometry() |>
  pivot_longer(cols = -ward_2017, values_to = "votes") |>
  inner_join(wards.2017.to.blocks.v2, relationship = "many-to-many") |>
  inner_join(block.vap |>
               select(block_2020, vap_2016)) |>
  group_by(ward_2017, name) |>
  mutate(ward_vap = sum(vap_2016),
         block_prop_of_ward = vap_2016/ward_vap,
         block_prop_of_ward = if_else(is.na(block_prop_of_ward), 0, block_prop_of_ward),
         # if any of a ward remains unallocated, add the remaining allocation to each
         #    constitutent block
         unallocated = 1 - sum(block_prop_of_ward, na.rm = T),
         block_prop_of_ward = block_prop_of_ward + unallocated/n(),
         disaggregated_votes = votes*block_prop_of_ward) |>
  ungroup()
sum(wards.to.blocks.2016$disaggregated_votes[wards.to.blocks.2016$name == "PRETOT16"], na.rm = T) == sum(ltsb.2017$PRETOT16, na.rm = T)

# 2014
wards.to.blocks.2014 <- ltsb.2014 |>
  select(ward_2014 = WARD_FIPS, ends_with("14")) |>
  st_drop_geometry() |>
  pivot_longer(cols = -ward_2014, values_to = "votes") |>
  inner_join(wards.2014.to.blocks.v2, relationship = "many-to-many") |>
  inner_join(block.vap |>
               select(block_2020, vap_2014)) |>
  group_by(ward_2014, name) |>
  mutate(ward_vap = sum(vap_2014),
         block_prop_of_ward = vap_2014/ward_vap,
         block_prop_of_ward = if_else(is.na(block_prop_of_ward), 0, block_prop_of_ward),
         # if any of a ward remains unallocated, add the remaining allocation to each
         #    constitutent block
         unallocated = 1 - sum(block_prop_of_ward, na.rm = T),
         block_prop_of_ward = block_prop_of_ward + unallocated/n(),
         disaggregated_votes = votes*block_prop_of_ward) |>
  ungroup()
sum(wards.to.blocks.2014$disaggregated_votes[wards.to.blocks.2014$name == "GOVTOT14"], na.rm = T) == sum(ltsb.2014$GOVTOT14, na.rm = T)

# 2012 (using 2011 wards)
wards.to.blocks.2012 <- ltsb.2011 |>
  select(ward_2011 = WARD_FIPS, ends_with("12")) |>
  st_drop_geometry() |>
  pivot_longer(cols = -ward_2011, values_to = "votes") |>
  inner_join(wards.2011.to.blocks.v2, relationship = "many-to-many") |>
  inner_join(block.vap |>
               select(block_2020, vap_2012)) |>
  group_by(ward_2011, name) |>
  mutate(ward_vap = sum(vap_2012),
         block_prop_of_ward = vap_2012/ward_vap,
         block_prop_of_ward = if_else(is.na(block_prop_of_ward), 0, block_prop_of_ward),
         # if any of a ward remains unallocated, add the remaining allocation to each
         #    constitutent block
         unallocated = 1 - sum(block_prop_of_ward, na.rm = T),
         block_prop_of_ward = block_prop_of_ward + unallocated/n(),
         disaggregated_votes = votes*block_prop_of_ward) |>
  ungroup()
sum(wards.to.blocks.2012$disaggregated_votes[wards.to.blocks.2012$name == "GOVTOT12"], na.rm = T) == sum(ltsb.2011$GOVTOT12, na.rm = T)
sum(wards.to.blocks.2012$disaggregated_votes[wards.to.blocks.2012$name == "PRETOT12"], na.rm = T) == sum(ltsb.2011$PRETOT12, na.rm = T)

###############################################################################

all.disaggregated.votes <- bind_rows(
  wards.to.blocks.2020 |>
    select(block_2020, name, disaggregated_votes),
  wards.to.blocks.2018 |>
    select(block_2020, name, disaggregated_votes),
  wards.to.blocks.2016 |>
    select(block_2020, name, disaggregated_votes),
  wards.to.blocks.2012 |>
    select(block_2020, name, disaggregated_votes)
)

# Verify that totals match Dave Leip's atlas of american elections
leip.totals <- tribble(
  ~leip, ~name,
  2516065, "GOVTOT12",
  3068434, "PRETOT12",
  2410314, "GOVTOT14",
  2976150, "PRETOT16",
  2673308, "GOVTOT18",
  3298041, "PRETOT20",
  2656490, "GOVTOT22"
)

# ALL MATCH
all.disaggregated.votes |>
  filter(str_detect(name, "TOT"),
         str_detect(name, "GOV|PRE")) |>
  group_by(name) |>
  summarise(total_votes = sum(disaggregated_votes, na.rm = T)) |>
  inner_join(leip.totals) |>
  mutate(match = total_votes == leip)

# just keep Dem, Rep, and Total votes for Gov, AG, Treasurer, Senator, and President
all.disaggregated.votes |>
  filter(str_sub(name, 1, 3) %in% c("GOV","WAG","WST","USS", "PRE"),
         str_sub(name, 4, -3) %in% c("DEM", "REP", "TOT"),
         #  remove race that has incorrect names. There was no AG race in 2012
         str_detect(name, "WAGTOT12|WAGDEM12|WAGREP12", negate = T)) |>
  write_csv("election-data/2012-2020/ltsb-disaggregated-to-2020-blocks_2012-20.csv.gz")

# Demonstration of how 2014 totals are incorrect in the 2011, 2014, and 2017 files
sum(ltsb.2014$GOVTOT14)
sum(ltsb.2011$GOVTOT14)
sum(ltsb.2017$GOVTOT14)
sum(ltsb.2018$GOVTOT14)

sum(ltsb.2014$AGTOT14)
sum(ltsb.2011$WAGTOT14)
sum(ltsb.2017$WAGTOT14)
sum(ltsb.2018$WAGTOT14)
