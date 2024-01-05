library(tidyverse)
library(gt)

# plan scores
demographics <- read_csv("analysis-r/tables/plan-demographics.csv")
contiguity <- read_csv("analysis-r/tables/plan-contiguity.csv")
splits <- read_csv("analysis-r/tables/plan-muni-and-county-splits.csv")
compactness <- read_csv("analysis-r/tables/plan-aggregate-compactness.csv")
partisanship <- read_csv("analysis-r/tables/plan-vote-margins.csv")

################################################################################
# prepare for table merging
demographics.2 <- demographics |>
  select(plan, total_pop_deviation = pct_deviation, black, hispanic, none)

contiguity.2 <- contiguity |>
  group_by(plan) |>
  summarise(noncontiguous_districts = sum(contiguous == "noncontiguous")) |>
  mutate(contiguity = if_else(noncontiguous_districts == 0,
                              "Yes",
                              paste0("No (", noncontiguous_districts, " splits)"))) |>
  select(plan, contiguity)

splits.2 <- splits

compactness.2 <- compactness |>
  select(plan, relative_perimeter)

partisanship.2 <- partisanship |>
  group_by(plan) |>
  arrange(modelled_outcome) |>
  filter((word(plan, -1, sep = "_") == "wsa" & row_number() == 50) |
           (word(plan, -1, sep = "_") == "wss" & row_number() == 17)) |>
  ungroup() |>
  select(plan, median_seat_lean = modelled_outcome)

################################################################################
# create table

combine <- demographics.2 |>
  inner_join(contiguity.2) |>
  inner_join(splits.2) |>
  inner_join(compactness.2) |>
  inner_join(partisanship.2) |>
  separate(plan, into = c("plan", "house"), sep = "_") |>
  mutate(
    plan = case_when(
      plan == "legis" ~ "GOP Legislature 2021",
      plan == "pmc" ~ "People's Map Commission 2021",
      plan == "everslc" ~ "Evers' Least Change 2021",
      TRUE ~ plan),
    house = if_else(house == "wss", "Senate", "Assembly"))

assembly.gt <- combine |>
  filter(house == "Assembly") |>
  select(-house) |>
  gt(rowname_col = "plan") |>
  tab_spanner(label = "Opportunity Districts", columns = 3:5) |>
  tab_spanner(label = "# split by districts", columns = 7:8) |>
  cols_label(
    total_pop_deviation = "Pop. deviation",
    black = "Black",
    hispanic = "Hispanic",
    none = "No majority",
    contiguity = "Contiguous?",
    split_municipalities = "municipalities",
    split_counties = "counties",
    relative_perimeter = "Relative compactness",
    median_seat_lean = "Tipping point seat"
  ) |>
  cols_width(
    plan ~ px(250),
    total_pop_deviation ~ px(90),
    black ~ px(50),
    hispanic ~ px(75),
    none ~ px(100),
    contiguity ~ px(110),
    split_municipalities ~ px(110),
    split_counties ~ px(90),
    relative_perimeter ~ px(110),
    median_seat_lean ~ px(100)
  ) |>
  fmt_percent(columns = total_pop_deviation, decimals = 2, scale_values = F) |>
  fmt_number(columns = relative_perimeter, decimals = 2) |>
  tab_header("Wisconsin Assembly Redistricting Plan Scorecard",
             subtitle = md(("using criteria outlined by the Wisconsin Supreme Court in *Rebecca Clarke v. Wisconsin Elections Commission*"))) |>
  tab_stubhead("plan submitted by") |>
  tab_style(style = cell_text(style = "italic"),
            locations = cells_stubhead()) |>
  tab_style(style = cell_text(weight = "bold", size = 16),
            locations = cells_title()) |>
  data_color(columns = total_pop_deviation, palette = "Oranges", domain = c(0,2)) |>
  data_color(columns = c(black, hispanic, none), palette = "Oranges", domain = c(0,8)) |>
  tab_style(style = cell_fill(color = "red"),
            locations = cells_body(columns = 6, rows = contiguity != "Yes")) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = 6, rows = contiguity == "Yes")) |>
  data_color(columns = split_municipalities, palette = "Oranges", domain = c(0, 200)) |>
  data_color(columns = split_counties, palette = "Oranges", domain = c(0, 72)) |>
  data_color(columns = relative_perimeter, palette = "Oranges", domain = c(1, 2)) |>
  data_color(columns = median_seat_lean, palette = "RdBu", domain = c(-17, 5)) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean < 0,
      fns = function(x){paste0("+", round(abs(x), 1), " Rep")}) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean > 0,
      fns = function(x){paste0("+", round(abs(x), 1), " Dem")}) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean == 0,
      fns = function(x){paste0("tie")}) |>
  tab_footnote(footnote = "Population deviation is the range between the most populous and least population districts, divided by the ideal district size.",
               locations = cells_column_labels("total_pop_deviation")) |>
  tab_footnote(footnote = "Number of districts where each group forms a majority of adults.",
               locations = cells_column_spanners("Opportunity Districts")) |>
  tab_footnote(footnote = "Only includes adults choosing non-Hispanic Black alone",
               local(cells_column_labels("black"))) |>
  tab_footnote(footnote = "Number of each split into multiple districts that themselves cross muni/county lines.",
               locations = cells_column_spanners("# split by districts")) |>
  tab_footnote(footnote = "The ratio of each plan's total perimeter from all districts, divided by the total perimeter from the shortest plan. A value of 1 indicates the most compact plan.",
               locations = cells_column_labels("relative_perimeter")) |>
  tab_footnote(footnote = "The modelled lean of the 50th seat in the 2022 Assembly elections, modelled using results from statewide races.",
               locations = cells_column_labels("median_seat_lean")) |>
  tab_source_note(md("Analysis by John D. Johnson, Marquette Law School Lubar Center Research Fellow. See [github.com/jdjohn215/wi-legis-map-proposals-2024](https://github.com/jdjohn215/wi-legis-map-proposals-2024/tree/main) for all methodological details, data, and code."))
assembly.gt

senate.gt <- combine |>
  filter(house == "Senate") |>
  select(-house) |>
  gt(rowname_col = "plan") |>
  tab_spanner(label = "Opportunity Districts", columns = 3:5) |>
  tab_spanner(label = "# split by districts", columns = 7:8) |>
  cols_label(
    total_pop_deviation = "Pop. deviation",
    black = "Black",
    hispanic = "Hispanic",
    none = "No majority",
    contiguity = "Contiguous?",
    split_municipalities = "municipalities",
    split_counties = "counties",
    relative_perimeter = "Relative compactness",
    median_seat_lean = "Tipping point seat"
  ) |>
  cols_width(
    plan ~ px(250),
    total_pop_deviation ~ px(90),
    black ~ px(50),
    hispanic ~ px(75),
    none ~ px(100),
    contiguity ~ px(110),
    split_municipalities ~ px(110),
    split_counties ~ px(90),
    relative_perimeter ~ px(110),
    median_seat_lean ~ px(100)
  ) |>
  fmt_percent(columns = total_pop_deviation, decimals = 2, scale_values = F) |>
  fmt_number(columns = relative_perimeter, decimals = 2) |>
  tab_header("Wisconsin State Senate Redistricting Plan Scorecard",
             subtitle = md(("using criteria outlined by the Wisconsin Supreme Court in *Rebecca Clarke v. Wisconsin Elections Commission*"))) |>
  tab_stubhead("plan submitted by") |>
  tab_style(style = cell_text(style = "italic"),
            locations = cells_stubhead()) |>
  tab_style(style = cell_text(weight = "bold", size = 16),
            locations = cells_title()) |>
  data_color(columns = total_pop_deviation, palette = "Oranges", domain = c(0,2)) |>
  data_color(columns = c(black, hispanic, none), palette = "Oranges", domain = c(0,6)) |>
  tab_style(style = cell_fill(color = "red"),
            locations = cells_body(columns = 6, rows = contiguity != "Yes")) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = 6, rows = contiguity == "Yes")) |>
  data_color(columns = split_municipalities, palette = "Oranges", domain = c(0, 200)) |>
  data_color(columns = split_counties, palette = "Oranges", domain = c(0, 72)) |>
  data_color(columns = relative_perimeter, palette = "Oranges", domain = c(1, 2)) |>
  data_color(columns = median_seat_lean, palette = "RdBu", domain = c(-17, 5)) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean < 0,
      fns = function(x){paste0("+", round(abs(x), 1), " Rep")}) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean > 0,
      fns = function(x){paste0("+", round(abs(x), 1), " Dem")}) |>
  fmt(columns = median_seat_lean,
      rows = median_seat_lean == 0,
      fns = function(x){paste0("tie")}) |>
  tab_footnote(footnote = "Population deviation is the range between the most populous and least population districts, divided by the ideal district size.",
               locations = cells_column_labels("total_pop_deviation")) |>
  tab_footnote(footnote = "Number of districts where each group forms a majority of adults.",
               locations = cells_column_spanners("Opportunity Districts")) |>
  tab_footnote(footnote = "Only includes adults choosing non-Hispanic Black alone",
               local(cells_column_labels("black"))) |>
  tab_footnote(footnote = "Number of each split into multiple districts that themselves cross muni/county lines.",
               locations = cells_column_spanners("# split by districts")) |>
  tab_footnote(footnote = "The ratio of each plan's total perimeter from all districts, divided by the total perimeter from the shortest plan. A value of 1 indicates the most compact plan.",
               locations = cells_column_labels("relative_perimeter")) |>
  tab_footnote(footnote = "The modelled lean of the 17th seat in the 2022 State Senate elections, modelled using results from statewide races.",
               locations = cells_column_labels("median_seat_lean")) |>
  tab_source_note(md("Analysis by John D. Johnson, Marquette Law School Lubar Center Research Fellow. See [github.com/jdjohn215/wi-legis-map-proposals-2024](https://github.com/jdjohn215/wi-legis-map-proposals-2024/tree/main) for all methodological details, data, and code."))
senate.gt

################################################################################
# save output
gtsave(assembly.gt, "scorecards/assembly-scorecard.png", vwidth = 1200)
gtsave(assembly.gt, "scorecards/assembly-scorecard.html")
gtsave(senate.gt, "scorecards/senate-scorecard.png", vwidth = 1200)
gtsave(senate.gt, "scorecards/senate-scorecard.html")
