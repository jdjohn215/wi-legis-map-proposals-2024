library(tidyverse)
library(sf)

# This script calculates various compactness scores for each plan
#   Some formulas are drawn from https://fisherzachary.github.io/public/r-output.html

################################################################################
# Setup
plan.paths <- list.files("plan-polygons", full.names = T, pattern = "wsa.geojson|wss.geojson")

read_plan_polygons <- function(path){
  plan <- st_read(path)
  plan |>
    mutate(plan = names(plan)[1]) |>
    rename(district = 1)
}

all.plan.polygons <- map_df(plan.paths, read_plan_polygons) |>
  mutate(perimeter = st_length(st_cast(geometry, "MULTILINESTRING")),
         area = st_area(geometry),
         centroid = st_centroid(geometry))

################################################################################
# Reock Score
#   ratio of district area to the area of the minimum bounding circle

all.plan.polygons.w.bounding.circle <- all.plan.polygons |>
  mutate(min_bound_circle = lwgeom::st_minimum_bounding_circle(geometry))

reock <- all.plan.polygons.w.bounding.circle |>
  mutate(bounding_circle_area = st_area(min_bound_circle),
         reock = as.numeric(area/bounding_circle_area)) |>
  st_drop_geometry() |>
  select(plan, district, reock)


################################################################################
# Polsby-Popper
#   ratio of the area of the district to the area of a circle whose circumfereance is equal to the perimeter of the district

polsby.popper <- all.plan.polygons |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(circle_area = pi * (perimeter / (2*pi))^2, # area of circle where circumference == district perimeter
         polsby_popper = as.numeric(area/circle_area)) |>
  select(plan, district, polsby_popper)

################################################################################
# Schwartzberg
#   ratio of the perimeter of the district to the circumference of a circle whoe area is equal to the area of the district

schwartzberg <- all.plan.polygons |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(schwartzberg = as.numeric(1/(perimeter / (2*pi*sqrt(area/pi))))) |>
  select(plan, district, schwartzberg)

################################################################################
# Convex hull
#   ratio of the area of the district to the area of the minimum convex polygon

convex.hull <- all.plan.polygons |>
  mutate(minimum_convey_hull = st_convex_hull(geometry),
         area_mch = st_area(minimum_convey_hull),
         convex_hull = as.numeric(area/area_mch)) |>
  st_drop_geometry() |>
  select(plan, district, convex_hull) |>
  as_tibble()

################################################################################
# Combine measures

all.plan.district.scores <- all.plan.polygons |>
  st_drop_geometry() |>
  as_tibble() |>
  select(plan, district, perimeter, area) |>
  inner_join(polsby.popper) |>
  inner_join(schwartzberg) |>
  inner_join(reock) |>
  inner_join(convex.hull)

all.plan.aggregate.scores <- all.plan.district.scores |>
  mutate(house = word(plan, -1, sep = "_")) |>
  group_by(house, plan) |>
  summarise(polsby_popper = mean(polsby_popper),
            schwartzberg = mean(schwartzberg),
            reock = mean(reock),
            convex_hull = mean(convex_hull),
            total_perimeter = sum(perimeter)) |>
  group_by(house) |>
  mutate(relative_perimeter = as.numeric(total_perimeter/min(total_perimeter))) |>
  ungroup()

################################################################################
# save output
write_csv(all.plan.district.scores, "analysis-r/tables/plan-seat-compactness.csv")
write_csv(all.plan.aggregate.scores, "analysis-r/tables/plan-aggregate-compactness.csv")

