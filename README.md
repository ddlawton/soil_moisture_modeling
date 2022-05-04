# README file for Cara et al. soil moisture GAM modeling paper

This README.md file was generated on 2022-02-14 by Douglas Lawton

GENERAL INFORMATION

1. Title of Dataset: Soil moisture analysis beween cover and non cover plots in North Carolina

2. Author Information
	+ Principal Investigator Contact Information
		Name: Cara Mathers
		Institution: NC State University
		Email: cmather@ncsu.edu

	+ Associate or Co-investigator Contact Information
		Name: Douglas Lawton
		Institution: NC State Universiy
		Email: ddlawton@ncsu.edu


3. Date of data collection (single date, range, approximate date) <suggested format YYYY-MM-DD>:

+2020-05-31 through 2021-09-31 (approximate)

4. Geographic location of data collection <latitude, longitude, or city/region, State, Country, as appropriate>: 

+Sandhills Research Station
+2148 Windblow Rd, Jackson Springs, NC 27281


SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: 

+Creative Commons Attribution 4.0 

2. Links to publications that cite or use the data: 

+TBA


6. Recommended citation for this dataset: 

+Mathers C., Lawton D., Huseth A., Suchoff D., and Woodley A. (2021). Soil moisture analysis between cover and non cover plots in North Carolina [Data set].


DATA & FILE OVERVIEW

+File List: 

```
.
├── code  
|   ├── Data_management_and_raw_viz_Dec15.R #Data management script
|   ├── Daily_modeling_04222022.R #The final modeling script and figure construction
├── data                    
│   ├── raw      
│   |   ├── Weather_station      # Weather station data from the research station
│   |   |   ├── QV2YC4V9_1.csv      
│   |   |   ├── QV2YC4V9_2.csv     
│   |   ├── Dryland21.csv       #Soil moisture data 
│   |   ├── Sandhills20.csv     #Soil moisture data 
│   |   ├── Irrigated21.csv     #Soil moisture data  
│   └── processed      
|        ├── combined_Dec282021.csv      # Final modeling data
├── output 
│   ├── Figure_1.png #Final publicaion figure
└── README.md

```


DATA-SPECIFIC INFORMATION FOR: [combined_Dec282021.csv]

1. Number of variables: 

+19 

2. Number of cases/rows: 

+404294

3. Variable List: 

* X5 = moisture at 5 centimeters
* X15 = moisture at 15 centimeters
* X20 = moisture at 30 cenitmeters
* TWS = total water storage
* Block = Randomized block design
* Trt = treatment (cover or non-cover)
* Boot = Pre or post boot plant stage (pre-boot - more open soils; post-boot larger plants and less open soils)
* Site = site plots irrigated or dryland (e.g. rainfall only) plots
* Date.x = Date (YYYY-MM-DD HH:MM:SS AM/PM)
* DOY = Day of year
* Hour = Hour of day
* Year = year
* MOD = Minute of day
* Air_temp = air temperatures (c) from nearest weather stations
* Precip = Precipitation  (mm) from nearest weather station
* Solar = Solar radiation from nearest weather station
* logger_mm = Precipitation recorded from a logger
* logger_rainclass = light/medium/heavy rainfall of logger records

4. Missing data: 

+Some soil sensors failed to record throughout the season and is denoted by NA in the dataset.


