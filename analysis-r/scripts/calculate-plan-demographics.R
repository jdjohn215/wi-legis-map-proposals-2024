library(tidyverse)

# This script calculates the population statistics for each plan

###########################################################################
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))
block.demographics <- read_csv("census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID))

plan.district.totals <- block.demographics |>
  inner_join(block.assignments) |>
  select(ends_with("wsa"), ends_with("wss"), pop, vap, vap_white, vap_black, vap_hisp) |>
  pivot_longer(cols = c(contains("wsa"), contains("wss")), names_to = "plan", values_to = "district") |>
  filter(district != "ZZZ") |>
  group_by(plan, district) |>
  summarise(across(.cols = where(is.numeric), .fns = sum), .groups = "drop") |>
  mutate(vap_majority = case_when(
    vap_white/vap > 0.5 ~ "white",
    vap_black/vap > 0.5 ~ "black",
    vap_hisp/vap > 0.5 ~ "hispanic",
    TRUE ~ "no majority"
  ))

plan.summary <- plan.district.totals |>
  group_by(plan) |>
  summarise(max = max(pop),
            min = min(pop),
            range = max - min,
            pct_deviation = (range/(sum(pop)/n()))*100,
            sd = sd(pop),
            black = sum(vap_majority == "black"),
            hispanic = sum(vap_majority == "hispanic"),
            none = sum(vap_majority == "no majority"))
write_csv(plan.summary, "analysis-r/tables/plan-demographics.csv")
