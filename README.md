## Background Plots
- [Sea Level Rise](https://climate.nasa.gov/vital-signs/sea-level/)
- [Global Temperature Rise](https://climate.nasa.gov/vital-signs/global-temperature/)
- [Artic Sea Ice Minimum](https://climate.nasa.gov/vital-signs/arctic-sea-ice/)
- [Greenland Ice Mass](https://climate.nasa.gov/vital-signs/ice-sheets/)
- [NYC Precipitation Increase](https://www.weather.gov/media/okx/Climate/CentralPark/monthlyannualprecip.pdf)
- [Relative sea level trend at the battery](https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8518750)

## Resiliency 
- [311 Street Flooding Complaints Map](https://data.cityofnewyork.us/resource/fhrw-4uyv)
- [NYC 2019 Summer Surface Temperature Map](https://www.usgs.gov/land-resources/nli/landsat/landsat-provisional-surface-temperature?qt-science_support_page_related_con=0#qt-science_support_page_related_con)
- [Vegetative Cover - NYC LiDAR 2017](https://data.cityofnewyork.us/Environment/Land-Cover-Raster-Data-2017-6in-Resolution/he6d-2qns)
- [SVI]()

## Energy & Emmissions
- [NYC Energy by Source 2017](https://www1.nyc.gov/site/sustainability/reports-and-data/publications.page)
- [NYS Energy by Source 2017](https://www.eia.gov/state/?sid=NY)

## Circular Economy

#### Potential for Landfill Diversion Sankey
- [DSNY Monthly Tonnage Data](https://data.cityofnewyork.us/City-Government/DSNY-Monthly-Tonnage-Data/ebb7-mvp5)
- [2017 Residential, School and NYCHA Waste Characterization Study](https://dsny.cityofnewyork.us/wp-content/uploads/2018/04/2017-Waste-Characterization-Study.pdf)
#### Curbside Organics Map
- [Community District Boundaries](https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4)
- [Current Organics Rollout Info](https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/residents/current-organics-rollout)
- [2018 Organics Rollout](https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/2018-organics-rollout)

## Green Jobs

#### Difficulties in Hiring Bar Chart
- [2018 NYSDERA New York Clean Energy Industry Report](https://www.nyserda.ny.gov/About/Publications/New-York-Clean-Energy-Industry-Report)

## Methodology 

### NYC 2019 Summer Surface Temperature Map

#### Data Structure

- data
  - input
    - Parks Properties (https://data.cityofnewyork.us/City-Government/Parks-Properties/k2ya-ucmv)
    - Airport Polygon (https://data.cityofnewyork.us/City-Government/Airport-Polygon/xfhz-rhsk)
    - landsat_st (from Landsat folder on G drive)
    - Ground_Monitor_Temps_NYC (from Landsat folder on G drive)

#### Getting Data

- US Landsat 4-8 ARD: [Provisional Surface Temperature (ST)](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-level-2-provisional-surface?qt-science_center_objects=0#qt-science_center_objects)
  1. Make an account at (https://earthexplorer.usgs.gov/)
  2. Install [Bulk Download Application](https://earthexplorer.usgs.gov/bulk)
  3. On Earth Exloper site search panel, select desired criteria:
      - Date Range: 2014 to 2020
      - Datasets: US Landsat 4-8 ARD
      - Tile grid horizontal: 29 (NYC)
      - Tile grid vertical: 7 (NYC)
        * search for tile grid [here](https://www.usgs.gov/media/images/conterminous-us-landsat-analysis-ready-data-ard-tiles)
  4. Follow [BIG DATA Download](https://blogs.fu-berlin.de/reseda/landsat-big-data-download/#3) instructions from the blog site (blogs.fu-berlin.de) 
     - Where the instructions say "Choose “Non-Limited Results” and “CSV” in order to export the metadata of every single file found to a csv-file (which is a text file)" choose "Comma (,) Delimited" format instead.
     
- Ground Monitor Temperature:
  1. Select your local stations. (Central Park, LaGuardia, Kennedy)
  [Local Climatological Data (LCD)](https://www.ncdc.noaa.gov/cdo-web/datatools/lcd)
  2. You need to add the data to your cart, then go to your cart, where you can select that you want a csv and subset to the dates you are interested in.
     - [Central Park](https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:94728/detail)

#### Validating Data
- Satellite Readings
  1. Cloud Cover: https://landsat.usgs.gov/landsat-8-cloud-cover-assessment-validation-data#Urban
  2. Comparing Ground Monitor temperatures to Satellite readings
  
### 311 Street Flooding Complaints

#### Normalization
- round lat/long for normalization - 3 decimal places is up to 110 meters
- [New York City Panel on Climate Change 2019 Report Chapter 2: New Methods for Assessing Extreme Temperatures, Heavy Downpours, and Drought](https://nyaspubs.onlinelibrary.wiley.com/doi/10.1111/nyas.14007)
- Like NPCC figure 2.11, normalizing street flooding complaints by number of 'all 311 calls within the same area'
- complaints relative to a space, as some areas are known to submit more complaints than others. Given 3400 complaints across city, 110 m may be appropriate range
