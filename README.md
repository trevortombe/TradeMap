# TradeMap
Displays two-way trade between Canada and the United States, for each province and lower-48 states, relative to each region's GDP.

All necsesary files are in the directory. 

- "TradeData.csv" contains the data
- "centroids.csv" is longitude/latitude coordinates for state/province centroids
- "CreateMap.R" loads the data, packages, and creates the map
- "map.png" is the output

Sources:

State-level exports and imports, to and from Canada: https://www.census.gov/foreign-trade/statistics/state/index.html

State-level GDP: https://www.bea.gov/iTable/index_regional.cfm

Province-level exports and imports, to and from the United States: https://www.ic.gc.ca/app/scr/tdst/tdo/crtr.html?&productType=NAICS&lang=eng

Province-level GDP: There is no official nominal GDP for 2016 yet. I use the current price 2015 GDP from http://www5.statcan.gc.ca/cansim/a26?lang=eng&id=3840038 and then infer the 2016 levels using RBC's latest provincial outlook. Specifically, the forecast nominal GDP growth for 2016 from http://www.rbc.com/economics/economic-reports/pdf/provincial-forecasts/provtbl.pdf 

The North American shapefile is from the USGS: https://catalog.data.gov/dataset/usgs-small-scale-dataset-north-american-atlas-political-boundaries-200406-shapefile 
