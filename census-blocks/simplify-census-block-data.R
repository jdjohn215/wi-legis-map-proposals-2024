library(tidyverse)

# This script combines the block definition file with ward definition corrections agreed to by the parties

################################################################################
# The source file with Franklin corrected ward (015B)
orig <- read_csv("census-blocks/WI_BLOCKS_withoutWater_2020_TIGER_PL94171_DOJ_Fields_WithFranklinCorrections.csv",
                 col_types = cols(.default = "c"))

# contains additional 'ward fragments' with inaccurate municipal-ward identifiers
appendixa <- readxl::read_excel("census-blocks/Appendix A - Joint Stipulation as to Redistricting Data.xlsx",
                                col_types = "text")

# verify only ward numbers, not municipalities, are changing
table(word(appendixa$`MUNICIPAL WARD LABEL`, 1, 3) == word(appendixa$`CORRECTED LABEL`, 1, 3))

# convert from 1 row per ward to 1 row per block
appendixa.blocks <- appendixa |>
  select(WARD_FIPS = `WARD GEOID`, corrected_WARD_FIPS = `CORRECTED GEOID`,
         corrected_ward_label = `CORRECTED LABEL`, BLOCKS) |>
  mutate(BLOCKS = str_remove_all(BLOCKS, coll("[")),
         BLOCKS = str_remove_all(BLOCKS, coll("]"))) |>
  separate(BLOCKS, into = c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12"),
           sep = ",") |>
  pivot_longer(cols = starts_with("b"), values_to = "BLOCKID") |>
  filter(!is.na(BLOCKID)) |>
  mutate(BLOCKID = str_squish(BLOCKID)) |>
  select(-name)

################################################################################
# combine original and corrections, then standardize column names
block.stats <- orig |>
  left_join(appendixa.blocks |>
              select(BLOCKID, corrected_WARD_FIPS)) |>
  mutate(corrected_WARD_FIPS = 
           case_when(
             WARD_FIPS == "5507927300015B" ~ "5507927300015B", # special case of Oak Creek Ward 015B
             !is.na(corrected_WARD_FIPS) ~ corrected_WARD_FIPS,
             TRUE ~ WARD_FIPS
           )) |>
  select(GEOID, CNTY_FIPS, MCD_FIPS, CNTY_NAME, MCD_NAME, CTV, muni_fips = COUSUBFP,
         WARD_FIPS, corrected_WARD_FIPS,
         pop = PERSONS, pop_hisp = HISPANIC, pop_white = WHITE, pop_black = BLACK,
         pop_asian = ASIAN, pop_aian = AMINDIAN, pop_nhpi = PISLAND, 
         pop_other = OTHER, pop_two = OTHERMLT,
         vap = PERSONS18, vap_hisp = HISPANIC18, vap_white = WHITE18, vap_black = BLACK18,
         vap_asian = ASIAN18, vap_aian = AMINDIAN18, vap_nhpi = PISLAND18, 
         vap_other = OTHER18, vap_two = OTHERMLT18) |>
  mutate(across(.cols = c(contains("pop"), contains("vap")), .fns = as.numeric))

# corrected items
block.stats |>
  filter(WARD_FIPS != corrected_WARD_FIPS)

################################################################################
# save output
write_csv(block.stats, "census-blocks/wi-blocks-simple.csv")
