rm(list = ls())

library(tidyverse)
library(sf)
library(leaflet)
library(igraph)

##############################################################################
# set-up
block.shp <- st_read("census-blocks/WI_BLOCKS_2020_TIGER_PL94171.geojson")
block.assignments <- read_csv("block-assignments/all-plans.csv", 
                              col_types = cols(.default = "c"))
block.demographics <- read_csv("census-blocks/wi-blocks-simple.csv") |>
  mutate(GEOID = as.character(GEOID)) |>
  select(GEOID, pop)

contiguity <- read_csv("analysis-r/tables/plan-contiguity.csv")

# discontiguous 2024 proposals
discontiguous.districts <- contiguity |>
  filter(str_detect(plan, "everslc|pmc|legis_", negate = T), # remove 2022 plans
         components > 1)

##############################################################################
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

leaflet_district <- function(district, data){
  dist.data <- data |>
    filter(dist_number == district) |>
    st_transform(crs = 4326)
  leaflet(data = dist.data |> filter(component == 1)) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(label = ~GEOID, popup = ~GEOID) |>
    addPolygons(data = dist.data |> filter(component > 1),
                label = ~paste(GEOID, pop, sep = ","), 
                popup = ~paste(GEOID, pop, sep = ","),
                fillColor = "red", color = "red")
}

##############################################################################
# Senate Democrats Assembly
sendems.blocks <- block.assignments |>
  select(GEOID, dist_number = sendems_wsa) |>
  filter(dist_number %in% discontiguous.districts$dist_number[discontiguous.districts$plan == "sendems_wsa"]) |>
  inner_join(block.shp) |>
  st_as_sf() |>
  inner_join(block.demographics)

sendems.block.components <- map_df(unique(sendems.blocks$dist_number),
                                   get_district_components,
                                   data = sendems.blocks, .progress = T)

sendems.blocks.w.components <- inner_join(sendems.blocks, sendems.block.components)
component.pops <- sendems.blocks.w.components |>
  st_drop_geometry() |>
  group_by(dist_number, component) |>
  summarise(component_pop = sum(pop), .groups = "drop") |>
  arrange(dist_number, desc(component_pop)) |>
  group_by(dist_number) |>
  mutate(component_reorder = row_number()) |>
  ungroup()

sendems.blocks.w.components.2 <- inner_join(sendems.blocks.w.components, component.pops) |>
  mutate(component = component_reorder)

sendems.blocks.w.components.2 |> filter(component > 1, pop > 0)

sort(unique(sendems.block.components$dist_number))

leaflet_district("44", sendems.blocks.w.components.2)
leaflet_district("45", sendems.blocks.w.components.2)
leaflet_district("47", sendems.blocks.w.components.2)
leaflet_district("48", sendems.blocks.w.components.2)
leaflet_district("91", sendems.blocks.w.components.2)
leaflet_district("92", sendems.blocks.w.components.2)
leaflet_district("98", sendems.blocks.w.components.2)

sendems.blocks.w.components.2 |>
  select(GEOID, dist_number, component, pop) |>
  st_write("census-blocks/SenateDemocrats_DiscontinguousDistricts_Assembly.geojson")

##############################################################################
# Senate Democrats Senate
sendems.blocks <- block.assignments |>
  select(GEOID, dist_number = sendems_wss) |>
  filter(dist_number %in% discontiguous.districts$dist_number[discontiguous.districts$plan == "sendems_wss"]) |>
  inner_join(block.shp) |>
  st_as_sf() |>
  inner_join(block.demographics)

sendems.block.components <- map_df(unique(sendems.blocks$dist_number),
                                   get_district_components,
                                   data = sendems.blocks, .progress = T)

sendems.blocks.w.components <- inner_join(sendems.blocks, sendems.block.components)
component.pops <- sendems.blocks.w.components |>
  st_drop_geometry() |>
  group_by(dist_number, component) |>
  summarise(component_pop = sum(pop), .groups = "drop") |>
  arrange(dist_number, desc(component_pop)) |>
  group_by(dist_number) |>
  mutate(component_reorder = row_number()) |>
  ungroup()

sendems.blocks.w.components.2 <- inner_join(sendems.blocks.w.components, component.pops) |>
  mutate(component = component_reorder)

sendems.blocks.w.components.2 |> filter(component > 1, pop > 0)

sort(unique(sendems.block.components$dist_number))

leaflet_district("16", sendems.blocks.w.components.2)
leaflet_district("33", sendems.blocks.w.components.2)

##############################################################################
# Legislative Republicans Assembly
# This demonstrates that legis24_wsa == 41 is ACTUALLY contiguous
#   some islands are separated by water AND
#   instead of leaving the water block (550471004003059) unassigned,
#   this plan assigns it to district 42. However, a literal island still counts
#   as contiguous, per SCOWIS ruling. I update this districts status to CONTIGUOUS
#   in the scorecard script.

legis24.blocks <- block.assignments |>
  select(GEOID, dist_number = legis24_wsa) |>
  filter(dist_number %in% discontiguous.districts$dist_number[discontiguous.districts$plan == "legis24_wsa"]) |>
  inner_join(block.shp) |>
  st_as_sf()

legis24.block.components <- map_df(unique(legis24.blocks$dist_number),
                                   get_district_components,
                                   data = legis24.blocks, .progress = T)

legis24.blocks.w.components <- inner_join(legis24.blocks, legis24.block.components)

sort(unique(legis24.block.components$dist_number))

leaflet_district("41", legis24.blocks.w.components)

block.shp |>
  filter(str_sub(GEOID, 1, 5) == "55047") |>
  inner_join(block.assignments |> select(GEOID, legis_wsa)) |>
  st_transform(crs = 4326) |>
  leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(label = ~paste(legis_wsa, GEOID, sep = ", "),
              popup = ~paste(legis_wsa, GEOID, sep = ", "))
