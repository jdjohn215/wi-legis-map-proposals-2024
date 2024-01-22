rm(list = ls())

library(tidyverse)
library(sf)

# This script models state legislative election outcomes in hypothetical districts

################################################################################
# Set-up the actual election data

# the original election results in the actual districts used in 2022
election.results <- st_read("election-data/ReportingUnitPolygons.geojson") |>
  st_drop_geometry() |>
  as_tibble()

election.results.by.district <- election.results |>
  pivot_longer(cols = c(assembly_district, senate_district),
               names_to = "house", values_to = "district") |>
  select(house, district, ends_with("22")) |>
  pivot_longer(cols = ends_with("22")) |>
  group_by(house, district, name) |>
  summarise(votes = sum(value),
            .groups = "drop") |>
  separate(col = name, into = c("office","party","year"), sep = c(3,-2)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(house = word(house, 1, sep = "_"),
         margin = (DEM/TOT - REP/TOT)*100)

contested.legis <- election.results.by.district |>
  filter((house == "assembly" & office == "WSA") |
           (house == "senate" & office == "WSS"),
         DEM > 0,
         REP > 0,
         !is.na(TOT)) |>
  select(house, district)

# margins for all elections in contested legislative races
margins.actual <- election.results.by.district |>
  select(house, district, office, margin) |>
  pivot_wider(names_from = office, values_from = margin) |>
  inner_join(contested.legis) |>
  mutate(actual = if_else(house == "assembly", WSA, WSS))


################################################################################
# build the model using the real election results

# Two-step modelling process
#   Use the "v1" model to predict Seat winner
#   Use the "v2" model to predict Seat margin using interaction term with predicted winner from "v1"
lm.margin.v1 <- lm(actual ~ GOV + USS + WAG + WST, data = margins.actual)

# demonstrate that any differences in structure of WSA and WSS races are not detectable
lm.margin.v1.1 <- lm(actual ~ GOV*hd + USS*hd + WAG*hd + WST*hd, data = margins.actual |>
                       mutate(hd = if_else(house == "senate", 1, 0)))
summary(lm.margin.v1.1)

margins.actual.v2 <- margins.actual %>%
  mutate(predicted_margin_v1 = predict.lm(lm.margin.v1, newdata = .[]),
         predicted_dem_win = if_else(predicted_margin_v1 > 0, 1, 0))

lm.margin.v2 <- lm(actual ~ predicted_dem_win + GOV*predicted_dem_win +
                     USS*predicted_dem_win + WAG*predicted_dem_win +
                     WST*predicted_dem_win, data = margins.actual.v2)


################################################################################
# compare the residuals from each model, as well as the simple average of the 4 statewide races

compare.residuals <- margins.actual.v2 %>%
  mutate(predict_v1 = predict.lm(lm.margin.v1, newdata = .[]),
         predict_v2 = predict.lm(lm.margin.v2, newdata = .[]),
         predict_avg = (GOV + USS + WAG + WST)/4,
         residuals_v1 = actual - predict_v1,
         residuals_v2 = actual - predict_v2,
         residuals_avg = actual - predict_avg) |>
  select(house, district, actual, starts_with("predict_"),
         starts_with("resid")) |>
  pivot_longer(cols = -c(house, district, actual)) |>
  separate(name, into = c("stat", "model"), sep = "_") |>
  pivot_wider(names_from = stat, values_from = value)

compare.residuals |>
  ggplot(aes(actual, residuals)) +
  geom_point() +
  ggrepel::geom_text_repel(data = function(x){filter(x, abs(residuals) > 5)},
                           aes(label = district), min.segment.length = 0.01) +
  facet_grid(rows = vars(house), cols = vars(model))

################################################################################
# predict results
votes.by.district <- read_csv("election-data/votes-in-proposed-districts_2012-22.csv") |>
  separate(col = race, into = c("office","party","year"), sep = c(3,-2)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(margin = (DEM/TOT - REP/TOT)*100)

votes.by.district.22 <- votes.by.district |> filter(year == "22")

predicted <- votes.by.district.22 |>
  select(plan, district, office, margin) |>
  pivot_wider(names_from = office, values_from = margin) %>%
  mutate(predict_v1 = predict.lm(lm.margin.v1, newdata = .[]),
         predicted_dem_win = if_else(predict_v1 > 0, 1, 0)) %>%
  mutate(predict_v2 = predict.lm(lm.margin.v2, newdata = .[]))

# total winners by plan
predicted |>
  group_by(plan) |>
  summarise(dem_seats = sum(predict_v2 > 0))

################################################################################
# save output
final.output <- inner_join(
  # 2022 results
  predicted |>
    select(plan, district, modelled_outcome_22 = predict_v2, GOV_22 = GOV, 
           USS_22 = USS, WAG_22 = WAG, WST_22 = WST),
  # 2012-2020
  votes.by.district |>
    filter(year < 22) |>
    select(plan, district, year, office, margin) |>
    pivot_wider(names_from = c(office, year), values_from = margin)
) |>
  select(plan, district, modelled_outcome_22, ends_with("22"),
         ends_with("20"), ends_with("18"), ends_with("16"), ends_with("14"),
         ends_with("12")) |>
  filter(district != "ZZZ")

write_csv(final.output, "analysis-r/tables/plan-vote-margins.csv")
