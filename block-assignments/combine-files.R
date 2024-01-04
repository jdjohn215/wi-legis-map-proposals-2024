library(tidyverse)


# This script combines each plan's block assignment file(s)

############################################################################
# People's Map Commission
pmc.assembly <- read_csv("block-assignments/PMC_Assembly.csv",
                         col_types = "cc") |>
  rename(pmc_wsa = district)
pmc.senate <- read_csv("block-assignments/PMC_Senate.csv",
                       col_types = "cc") |>
  rename(pmc_wss = district)

############################################################################
# Legislature's plan (ultimately selected by SCOWIS & used in the 2022 election)
legis.assembly <- read_delim("block-assignments/55_WI_SLDL22.txt", delim = ",",
                             col_types = "cc") |>
  rename(legis_wsa = SLDLST) |>
  mutate(legis_wsa = if_else(legis_wsa != "ZZZ", as.character(as.numeric(legis_wsa)), legis_wsa))
legis.senate <- read_delim("block-assignments/55_WI_SLDU22.txt", delim = ",",
                             col_types = "cc") |>
  rename(legis_wss = SLDUST) |>
  mutate(legis_wss = if_else(legis_wss != "ZZZ", as.character(as.numeric(legis_wss)), legis_wss))

############################################################################
# combine all the plans into a single object
#   1 row per block, 1 column per plan/house
all.plans <- pmc.assembly |>
  left_join(pmc.senate) |>
  left_join(legis.assembly) |>
  left_join(legis.senate)

write_csv(all.plans, "block-assignments/all-plans.csv")
