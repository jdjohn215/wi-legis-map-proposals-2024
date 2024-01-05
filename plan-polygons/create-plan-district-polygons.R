library(tidyverse)
library(sf)

# This script dissolves census block polygons into district polygons for each plan

# Prepare data
block.shp <- st_read("census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson") |>
  inner_join(read_csv("block-assignments/all-plans.csv", 
                      col_types = cols(.default = "c"))) |>
  filter(legis_wsa != "ZZZ")
state.boundary <- st_read("plan-polygons/Wisconsin_State_Boundary_24K.geojson") |>
  st_transform(crs = 4326) |>
  select(geometry)

create_district_polygons <- function(plan_name){
  plan.polygons <- block.shp |>
    group_by(!!sym(plan_name)) |>
    summarise(geometry = st_union(geometry, is_coverage = TRUE)) |>
    st_make_valid() |>
    st_transform(crs = 4326) |>
    # trim
    st_intersection(state.boundary) |>
    st_make_valid() |>
    group_by(!!sym(plan_name)) |>
    summarise(geometry = st_union(geometry, is_coverage = TRUE)) |>
    st_make_valid()
  st_write(plan.polygons, paste0("plan-polygons/", plan_name, ".geojson"))
}

map(.x = setdiff(names(block.shp), c("GEOID", "geometry", str_remove(list.files("plan-polygons"), ".geojson"))),
    .f = create_district_polygons)
