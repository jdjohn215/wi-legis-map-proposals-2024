# Tabulating votes in alternative proposed districts

This document describes my methodology for allocating historical election results into alternative proposed legislative districts. My approach varies slightly from the method used by DavesRedistricting.org (DRA).

The following basic facts underline the challenge of allocating election results in Wisconsin.

1. Votes in Wisconsin are counted in *reporting units*, which are single wards in large municipalities and often combinations of wards in smaller municipalities.
2. The state does not publish a reporting unit shapefile.
3. The state Legislative Technology Services Bureau (LTSB) collects ward boundaries twice a year. Ward boundaries can change between elections, so regular collection is needed. The LTSB then [*disaggregates*](https://geodata.wisc.edu/catalog/WILTSB-b411164fe264496c8b1a1b5c5d0c0b8d0) reporting unit election totals, first into the wards used in that election, then into the census blocks constituting those wards (based on total population). To transfer the results from one set of wards into another, they then reaggregate from census blocks up into the final set of ward boundaries.

These multiple steps of *dis* and *re*aggregating votes inevitably introduces some uncertainty about vote totals in specific geographies. Most of the time this doesn't matter, because most wards aren't split between districts--no matter how you define them. But when a newly proposed district *does* split a ward errors from various disaggregation decisions can add up.

As I understand it, DRA takes LTSB ward data, then disaggregates once again to census blocks. Once a plan is provided in the form of a block assignment file, it reaggregates the votes into that plan.

My methodology is similar, but varies in some ways that may result in slightly more accurate vote counts. For one thing, DRA's vote totals for the 2016 presidential election are 3,929 votes short of the official statewide total, while my total matches.

# 2022 results

I [assembled a unique reporting-unit GIS file](https://github.com/jdjohn215/wi-nov-2022) for the 2022 election by merging the polygons of the individual wards which constituted each reporting unit. Then, I disaggregated votes from those wards into census blocks using actual geocoded voter locations from the 2022 voter file. This should make my results more accurate than DRA's because it involves a single disaggregation step (not two) and it uses actual voter data (not census counts) as the allocation weights.

# 2012-2020 results

I do not have reporting unit boundaries for previous elections, so I also rely on LTSB data prior to 2022. However, rather than using a single LTSB file, e.g. "2012-2020_Election_Data_with_2020_Wards", I attempt to use the LTSB file with ward boundaries closest in time to the given election. I obtained these old LTSB files the [GeoData&Wisconsin](https://geodata.wisc.edu/) archive.

Here are citations for the specific files I used. I attemped to use [2014 file](https://geodata.wisc.edu/catalog/2AD58FD2-EEF6-4484-B438-C2E649BD0361); however, this file contains incorrect vote totals for at least some elections.

* Wisconsin Legislative Technology Services Bureau. (2020). *2012-2020 Election Data (with 2020 Wards)*, Wisconsin, 2020. [https://geodata.wisc.edu/catalog/317F4F49-5B17-43CC-9BCA-36ED25DC9E15](https://geodata.wisc.edu/catalog/317F4F49-5B17-43CC-9BCA-36ED25DC9E15)
  * I used this file for the 2020 election results.
* Wisconsin Legislative Technology Services Bureau. (2018). *2012-2020 Election Data (with 2018 Wards)*, Wisconsin 2018. [https://geodata.wisc.edu/catalog/B41117FE-B7E9-423A-91C8-E0DF28ACA065](https://geodata.wisc.edu/catalog/B41117FE-B7E9-423A-91C8-E0DF28ACA065) 
  * I used this file for the 2018 and 2014 election results (because it contained correct totals for the 2014 races).
* Wisconsin Legislative Technology Services Bureau. (2017). *2012-2020 Election Data (with 2017 Wards)*, Wisconsin 2017. [https://geodata.wisc.edu/catalog/145055E1-87EF-4D13-B138-4DC3907F3677](https://geodata.wisc.edu/catalog/145055E1-87EF-4D13-B138-4DC3907F3677)
  * I used this file for the 2016 election results.
* Wisconsin Legislative Technology Services Bureau. (2011). *2012-2020 Election Data (with 2011 Wards)*, Wisconsin 2011. [https://geodata.wisc.edu/catalog/731B8F17-F2D7-48DC-A4FC-616ACC331E7A](https://geodata.wisc.edu/catalog/731B8F17-F2D7-48DC-A4FC-616ACC331E7A)
  * I used this file for the 2012 election results (including the gubernatorial recall).

I assigned census blocks to wards by intersecting the centroids of each census block with wards. In a handful of instances from each year, this resulted in very small wards not being assigned any census blocks. In those cases, I reassigned the census block containing the centroid of the ward to that ward.

Once each census block was uniquely assigned to a single ward, I disaggregated the ward's election results to the blocks based on the share of the ward's adult population residing in each census block. See [this script](election-data/2012-2020/disaggregate-ltsb-to-2020-blocks.R) for documented replication code. I determined census block adult population by linearly interpolating adult population in 2010 to adult population in 2020. I obtained the 2010 adult population for vintage 2020 census blocks boundaries by disaggregating 2010 blocks into 2020 blocks using land area overlap in the Census Bureau's official intercensal block relationship file. This is an inexact method, but it should be more directionally correct than simply using the 2020 adult population for each year of the decade. See [this script](census-blocks/interpolate-block-vap.R) for documented replication code.

As with DRA's method, my allocation can still result in votes being assigned to portions of the state that aren't in any district, but this is limited to a trivial number.

# Access the data

Election results for presidential, gubernatorial, US senate, attorney general, and state treasurer races, allocated according to my methods described above, are available to download from this repository.

* [this file](election-data/votes-in-proposed-districts_2012-22.csv) contains the Democratic, Republican, and total votes for each district in each plan.
* [this file](analysis-r/tables/plan-vote-margins.csv) contains just the two-party Democratic vote margin (Democratic % minus Republican %).

In each file, I use the following office abbreviations:

* GOV - Governor
* USS - US Senate
* PRE - President
* WAG - Wisconsin Attorney General
* WST - Wisconsin State Treasurer

The plans are names as follows. The suffix "_wsa" indicates Wisconsin State Assembly and "_wss" indicates a Wisconsin State Senate map.

* everslc     - Evers Least Change Map (initially chosen by the state Supreme Court in 2022 before being rejected by the federal Supreme Court)
* legis       - the Legislature's map, ultimately used in the 2022 election
* pmc         - the People's Map Commission final submission to the WI Supreme Court
* evers24     - Evers 2024 submission
* wright      - the Wright Petitioners submission
* lawforward  - the Law Forward submission, "Clarke Petitions"
* will        - the Wisconsin Institute for Law and Liberty submission, "Johnson Intervenors"
* sendems     - the submission from several Democratic State Senators
* petering    - the submission from Matt Petering using his "FastMap" algorithm. It was ultimately rejected for consideration by the Court because Petering was not one of the original petitioners on the case.
* legis24     - the Legislative Republicans submission

