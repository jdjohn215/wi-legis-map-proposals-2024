library(tidyverse)
library(sf)
library(igraph)

# This script checks the contiguity of the districts in each plan
#   it can take many minutes to run

###############################################################################
block.shp <- st_read("census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson")
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))

block.shps.with.assignments <- inner_join(block.shp, block.assignments)

# identify component of membership for each block in a given district
get_district_components <- function(district, data){
  
  # keep blocks for the specific district, along with unassigned water
  district.blocks <- data |>
    filter(dist_number %in% c(district, "ZZZ")) |>
    select(GEOID, dist_number)
  
  # create adjacency matrix for all the blocks in the district
  district.adj <- district.blocks |>
    tmaptools::get_neighbours()
  
  # identify the component(s) from the adjancency list
  district.components <- igraph::graph_from_adj_list(district.adj) |>
    components()
  
  # add components to the blocks in the district
  district.blocks |>
    mutate(component = district.components$membership) |>
    # remove water only blocks not actually assigned to the district
    filter(dist_number != "ZZZ") |>
    # ensure that components are numbered sequential, beginning with 1
    mutate(component = as.numeric(as.factor(component)),
           district = district) |>
    st_drop_geometry() |>
    select(GEOID, dist_number, component) |>
    tibble()
}

# identify the components for each district in a given plan
get_all_plan_districts <- function(plan, data = block.shps.with.assignments){
  plan.block.assignments <- block.shps.with.assignments |>
    select(GEOID, sym(plan)) |>
    rename(dist_number = 2)
  
  map_df(.x = setdiff(unique(plan.block.assignments$dist_number), "ZZZ"),
         .f = get_district_components,
         data = plan.block.assignments,
         .progress = TRUE) |>
    mutate(plan = plan)
}

# apply the function to each plan
all.plan.district.components <- map_df(.x = setdiff(names(block.assignments), "GEOID"),
                                       .f = get_all_plan_districts,
                                       .progress = T)

plan.district.status <- all.plan.district.components |>
  group_by(plan, dist_number) |>
  summarise(components = n_distinct(component), .groups = "drop") |>
  mutate(contiguous = ifelse(components == 1, "contiguous", "noncontiguous"))

plan.district.status |>
  group_by(plan, contiguous) |>
  summarise(districts = n()) |>
  pivot_wider(names_from = contiguous, values_from = districts)

write_csv(plan.district.status, "analysis-r/tables/plan-contiguity.csv")
