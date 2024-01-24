rm(list = ls())

library(tidyverse)
library(gt)

margins <- read_csv("analysis-r/tables/plan-vote-margins.csv")

margins.summary <- margins |>
  group_by(plan) |>
  summarise(`Modelled '22 Legis._Dem` = sum(modelled_outcome_22 > 0),
            `Modelled '22 Legis._Rep` = sum(modelled_outcome_22 < 0),
            `Tony Evers_Dem` = sum(GOV_22 > 0),
            `Tony Evers_Rep` = sum(GOV_22 < 0),
            `Ron Johnson_Dem` = sum(USS_22 > 0),
            `Ron Johnson_Rep` = sum(USS_22 < 0),
            tossup = sum(GOV_22 > 0 & USS_22 < 0)) |>
  separate(plan, into = c("plan","house"), sep = "_") |>
  mutate(
    era = if_else(plan %in% c("legis","pmc","everslc"), "old submissions (2022)", "new submissions (2024)"),
    plan = case_when(
      plan == "legis" ~ "GOP Legislature 2021",
      plan == "pmc" ~ "People's Map Commission 2021",
      plan == "everslc" ~ "Evers' Least Change 2021",
      plan == "legis24" ~ "Legislative Republicans",
      plan == "evers24" ~ "Evers 2024",
      plan == "wright" ~ "Wright Petitioners",
      plan == "sendems" ~ "Senate Democrats",
      plan == "petering" ~ "Petering (FastMap)",
      plan == "will" ~ "W.I.L.L.",
      plan == "lawforward" ~ "Law Forward",
      TRUE ~ plan),
    house = if_else(house == "wss", "Senate", "Assembly"))

assembly.gt <- margins.summary |>
  filter(house == "Assembly") |>
  select(-house) |>
  group_by(era) |>
  arrange(`Modelled '22 Legis._Dem`) |>
  gt(rowname_col = "plan") |>
  tab_spanner_delim("_") |>
  data_color(columns = contains("Rep"), palette = "Reds", domain = c(35,64)) |>
  data_color(columns = contains("Dem"), palette = "Blues", domain = c(35,64)) |>
  data_color(columns = tossup, palette = "Greys", domain = c(2,9)) |>
  tab_style(style = cell_borders(sides = "left", weight = "2px", color = "white"),
            locations = cells_body(columns = contains("Dem"))) |>
  tab_style(style = cell_borders(sides = "right", weight = "2px", color = "white"),
            locations = cells_body(columns = contains("Rep"))) |>
  cols_label(tossup = "toss-up") |>
  tab_header(title = "2022 WI State Assembly outcomes under various proposed redistricting plans") |>
  tab_footnote(footnote = "The model uses the governor, senate, state treasurer, & attorney general results to predict the state legislative result in each hypothetical district. In the model, as in reality, legislative Republicans did slightly better than Ron Johnson.",
               locations = cells_column_spanners(1)) |>
  tab_footnote(footnote = md("Seats won by **both** Tony Evers and Ron Johnson"),
               locations = cells_column_labels("tossup")) |>
  tab_footnote(footnote = "On January 17, the Court ruled that the would not consider the Petering submission, as Petering was not among the original petitioners in the case.",
               locations = cells_stub(plan == "Petering (FastMap)")) |>
  tab_source_note("All calculations by John D. Johnson (@jdjmke). See github.com/jdjohn215/wi-legis-map-proposals-2024 for full data and methodological details.") |>
  tab_options(table.width = 700)

senate.gt <- margins.summary |>
  filter(house == "Senate") |>
  select(-house) |>
  group_by(era) |>
  arrange(`Modelled '22 Legis._Dem`) |>
  gt(rowname_col = "plan") |>
  tab_spanner_delim("_") |>
  data_color(columns = contains("Rep"), palette = "Reds", domain = c(10,23)) |>
  data_color(columns = contains("Dem"), palette = "Blues", domain = c(10,23)) |>
  data_color(columns = tossup, palette = "Greys", domain = c(0,4)) |>
  tab_style(style = cell_borders(sides = "left", weight = "2px", color = "white"),
            locations = cells_body(columns = contains("Dem"))) |>
  tab_style(style = cell_borders(sides = "right", weight = "2px", color = "white"),
            locations = cells_body(columns = contains("Rep"))) |>
  cols_label(tossup = "toss-up") |>
  tab_header(title = "2022 WI State Senate outcomes under various proposed redistricting plans") |>
  tab_footnote(footnote = "The model uses the governor, senate, state treasurer, & attorney general results to predict the state legislative result in each hypothetical district. In the model, as in reality, legislative Republicans did slightly better than Ron Johnson.",
               locations = cells_column_spanners(1)) |>
  tab_footnote(footnote = md("Seats won by **both** Tony Evers and Ron Johnson"),
               locations = cells_column_labels("tossup")) |>
  tab_footnote(footnote = "On January 17, the Court ruled that the would not consider the Petering submission, as Petering was not among the original petitioners in the case.",
               locations = cells_stub(plan == "Petering (FastMap)")) |>
  tab_source_note("All calculations by John D. Johnson (@jdjmke). See github.com/jdjohn215/wi-legis-map-proposals-2024 for full data and methodological details.") |>
  tab_options(table.width = 700)
# save output
gtsave(assembly.gt, "scorecards/assembly-margins-scorecard.png", vwidth = 700)
gtsave(assembly.gt, "scorecards/assembly-margins-scorecard.html")
gtsave(senate.gt, "scorecards/senate-margins-scorecard.png", vwidth = 700)
gtsave(senate.gt, "scorecards/senate-margins-scorecard.html")
