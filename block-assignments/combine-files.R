library(tidyverse)


# This script combines each plan's block assignment file(s)


############################################################################
# Evers 2024
evers24.assembly <- read_csv("block-assignments/Evers 2024 Assembly Proposal.csv",
                             col_types = "cc") |>
  rename(GEOID = GEOID20, evers24_wsa = District)
evers24.senate <- read_csv("block-assignments/Evers 2024 Senate Proposal.csv",
                             col_types = "cc") |>
  rename(GEOID = GEOID20, evers24_wss = District)

############################################################################
# Petering (Fastmap)
petering.assembly <- read_csv("block-assignments/Petering Assembly.csv",
                             col_types = "cc") |>
  rename(GEOID = GEOID20, petering_wsa = District)
petering.senate <- read_csv("block-assignments/Petering Senate.csv",
                           col_types = "cc") |>
  rename(GEOID = GEOID20, petering_wss = District)

############################################################################
# Wright Petitioners
wright.assembly <- read_csv("block-assignments/Wright_Assembly_Map.csv",
                             col_types = "cc") |>
  rename(wright_wsa = 2)
wright.senate <- read_csv("block-assignments/Wright_Senate_Map.csv",
                            col_types = "cc") |>
  rename(wright_wss = 2)

############################################################################
# Senate Democrats
sendems.assembly <- read_csv("block-assignments/Senate Democrats Assembly Proposal.csv",
                             col_types = "cc") |>
  rename(GEOID = GEOID20, sendems_wsa = District)
sendems.senate <- read_csv("block-assignments/Senate Democrats Senate Proposal.csv",
                           col_types = "cc") |>
  rename(GEOID = GEOID20, sendems_wss = District)

############################################################################
# WI Legislature's Contiguity Correction plan
legis24 <- read_csv("block-assignments/WI Legislature Assembly Proposal.csv",
                    col_types = "cc") |>
  rename(GEOID = BLOCKID, legis24_wsa = District) |>
  # add senate districts using standard assembly-to-senate combinations
  inner_join(tibble(legis24_wsa = as.character(1:99),
                    legis24_wss = as.character(rep(1:33, each = 3))))

############################################################################
# Johnson Intervenors (WILL)
will.assembly <- read_csv("block-assignments/Johnson Intervenors Assembly Map - WILL - Block Assignments.csv",
                          col_types = "cc") |>
  rename(GEOID = GEOID20, will_wsa = District)
will.senate <- read_csv("block-assignments/Johnson Intervenors Senate Map - WILL - Block Assignments.csv",
                          col_types = "cc") |>
  rename(GEOID = GEOID20, will_wss = District)

############################################################################
# Clarke Petitioners (Law Forward)
lawforward.assembly <- read_csv("block-assignments/Clarke Petitioners Assembly - Law Forward.csv",
                                col_types = "cc") |>
  rename(GEOID = GEOID20, lawforward_wsa = District)
lawforward.senate <- read_csv("block-assignments/Clarke Petitioners Senate - Law Forward.csv",
                              col_types = "cc") |>
  rename(GEOID = GEOID20, lawforward_wss = District)

############################################################################
# People's Map Commission
pmc.assembly <- read_csv("block-assignments/PMC_Assembly.csv",
                         col_types = "cc") |>
  rename(pmc_wsa = district)
pmc.senate <- read_csv("block-assignments/PMC_Senate.csv",
                       col_types = "cc") |>
  rename(pmc_wss = district)


############################################################################
# Governor Evers' Least Change Plan
evers.lc.assembly <- read_csv("block-assignments/Governor's LC State Assembly.csv",
                         col_types = "cc") |>
  rename(GEOID = 1, everslc_wsa = District)
evers.lc.senate <- read_csv("block-assignments/Governor's LC State Senate.csv",
                       col_types = "cc") |>
  rename(GEOID = 1, everslc_wss = District)

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
  left_join(legis.senate) |>
  left_join(evers.lc.assembly) |>
  left_join(evers.lc.senate) |>
  left_join(will.assembly) |>
  left_join(will.senate) |>
  left_join(legis24) |>
  left_join(lawforward.assembly) |>
  left_join(lawforward.senate) |>
  left_join(evers24.assembly) |>
  left_join(evers24.senate) |>
  left_join(wright.assembly) |>
  left_join(wright.senate) |>
  left_join(sendems.assembly) |>
  left_join(sendems.senate) |>
  left_join(petering.assembly) |>
  left_join(petering.senate) |>
  # replace unassigned block NA values with "ZZZ"
  mutate(across(.cols = contains("_"),
                .fns = ~replace_na(.x, "ZZZ")))

write_csv(all.plans, "block-assignments/all-plans.csv")
