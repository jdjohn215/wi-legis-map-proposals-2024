library(tidyverse)

# This script calculates the population statistics for each plan

###########################################################################
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))
block.demographics <- read_csv("census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID))

plan.district.totals <- block.demographics |>
  inner_join(block.assignments) |>
  select(ends_with("wsa"), ends_with("wss"), starts_with("pop")) |>
  pivot_longer(-starts_with("pop"), names_to = "plan", values_to = "district") |>
  filter(district != "ZZZ") |>
  group_by(plan, district) |>
  summarise(across(.cols = where(is.numeric), .fns = sum), .groups = "drop") |>
  pivot_longer(cols = starts_with("pop_"), names_to = "subgroup") |>
  group_by(plan, district) |>
  mutate(largest_group = word(subgroup[value == max(value)], -1, sep = "_"),
         largest_share = max(value)/unique(pop)*100) |>
  pivot_wider(names_from = subgroup, values_from = value) |>
  ungroup()

plan.summary <- plan.district.totals |>
  group_by(plan) |>
  summarise(max = max(pop),
            min = min(pop),
            range = max - min,
            pct_deviation = (range/(sum(pop)/n()))*100,
            sd = sd(pop),
            black = sum(largest_group == "black"),
            hispanic = sum(largest_group == "hisp"))
write_csv(plan.summary, "analysis-r/tables/plan-demographics.csv")
