# Workflow

## Data sources and initial setup

* obtain the original census block boundary file from the Census Bureau. It's important to use the official file (not a simplified one) so that contiguity calculations are accurate. This file is in `census-blocks/WI_Blocks_2020_TIGER_PL94171.geojson`. It is too large to push to the github repo.
* block-level demographic data from the 2020 census. I use a file provided by the [Wisconsin Legislative Technology Services Burea](https://gis-ltsb.hub.arcgis.com/pages/download-data) with pre-summarized U.S. DOJ fields. The file is in `census-blocks/WI_BLOCKS_withoutWater_2020_TIGER_PL94171_DOJ_Fields_WithFranklinCorrections.csv`.
    * This file includes a new ward assignment for the City of Franklin. Details are in the [Joint Stipulation](https://www.wicourts.gov/courts/supreme/origact/docs/23ap1399_0102stip.pdf) file 1/2/24.
    * `Appendix A - Joint Stipulation as to Redistricting Data.xlsx` contains the Appendix of ward corrections also included in the Joint Stipulation.
    * The file `census-blocks/simplify-census-block-data.R` combines the corrections and standardizes column names, the output being `census-blocks/wi-blocks-simple.csv`.
* The basis of the election result statistics is a unique 2022 reporting unit GIS file, which I previously constructed as detailed in [this repository](https://github.com/jdjohn215/wi-nov-2022).
    * I intersect verified Nov. 2022 voter coordinates from the L2 Wisconsin voter file with both reporting units and census blocks. I use these intersected voters to create allocation factors between each census block and reporting unit. The output of this process is `election-data/blocks-to-reporting-units-allocation-factors.csv`.
    * I also allocate the disaggregated 2012-2020 election results in 2020 wards LTSB file into the new districts using a similar process. I assign each census block to a ward. Then I calculate the proportion of the ward belonging to the block using 2020 VAP. I use these allocation factors to assign 2020 LTSB ward data into new districts.


##  Processing data

1. Add new block assignments via `block-assignments/combine-files.R`
2. Allocate votes to districts via `election-data/allocate-votes-to-districts.R`
3. Make standardized district polygons for each plan using `plan-polygons/create-plan-district-polygons.R`

## Calculations

1. Calculate population deviation and VRA minority opportunity districts by running `analysis-r/scripts/calculate-plan-demographics.R`
2. Calculate contiguity by running `analysis-r/scripts/calculate-contiguity.R`
3. Count the number of county and municipality splits by running `analysis-r/scripts/calculate-splits.R`
4. Calculate various measures of compactness by running `analysis-r/scripts/calculate-compactness.R`
5. Model partisan tilt of each seat in each plan by running `analysis-r/scripts/model-votes.R`.

## Assemble Scorecard

1. run `analysis-r/scripts/build-scorecards.R`