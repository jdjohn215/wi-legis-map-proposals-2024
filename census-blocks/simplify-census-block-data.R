library(tidyverse)

block.stats <- read_csv("census-blocks/WI_Blocks_2020_PL94171_DOJ_Fields.csv",
         col_types = cols(.default = "c")) |>
  mutate(muni_fips = str_sub(MCD_FIPS, 6, -1)) |>
  select(GEOID = GEOID20, CNTY_FIPS, MCD_FIPS, CNTY_NAME, MCD_NAME, CTV, muni_fips,
         pop = PERSONS, pop_hisp = HISPANIC, pop_white = WHITE, pop_black = BLACK,
         pop_asian = ASIAN, pop_aian = AMINDIAN, pop_nhpi = PISLAND, 
         pop_other = OTHER, pop_two = OTHERMLT,
         vap = PERSONS18, vap_hisp = HISPANIC18, vap_white = WHITE18, vap_black = BLACK18,
         vap_asian = ASIAN18, vap_aian = AMINDIAN18, vap_nhpi = PISLAND18, 
         vap_other = OTHER18, vap_two = OTHERMLT18) |>
  mutate(across(.cols = c(contains("pop"), contains("vap")), .fns = as.numeric))

write_csv(block.stats, "census-blocks/wi-blocks-simple.csv")
