library(dplyr)
library(datapkg)
library(readxl)
library(tidyr)

##################################################################
#
# Processing Script for Demolitions
# Created by Jenna Daly
# On 08/16/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

demo_data <- dir(path_to_raw, recursive=T, pattern = "xls")

demo_df <- read_excel(paste0(path_to_raw, "/", demo_data), sheet=1, skip=5)

#Remove blank rows
demo_df <- demo_df[rowSums(is.na(demo_df)) != ncol(demo_df),]

newNames <- c("Town", "County", names(demo_df)[3:length(names(demo_df))])
names(demo_df) <- newNames

#Rename Total Demolitions to Connecticut
demo_df$Town[demo_df$Town == "Total Demolitions"] <- "Connecticut"

#convert to long format
last_col <- ncol(demo_df)
demo_df_long <- gather(demo_df, Year, Value, 3:last_col, factor_key=FALSE)

#Merge in town FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
town_fips <- (town_fips_dp$data[[1]])

demo_df_long_fips <- merge(demo_df_long, fips, by = "Town", all.y=T)

# Pull out data aggregated to county level
county <- demo_df_long_fips[!is.na(demo_df_long_fips$County),]
county$Town <- NULL
county$FIPS <- NULL
county <- county %>% 
  group_by(County, Year) %>% 
  summarise(Value = sum(Value, na.rm=T))

# County names should include the word "County"
county$County <- gsub("$", " County", county$County)

#Merge in county FIPS
county_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-county-list/master/datapackage.json'
county_fips_dp <- datapkg_read(path = county_fips_dp_URL)
county_fips <- (county_fips_dp$data[[1]])

county_with_fips <- merge(county, county_fips, by = "County", all.x=T)

# Change county column to "Town/County" in both sets
# Remove other geo columns
names(county_with_fips)[names(county_with_fips) == "County"] <- "Town/County"
names(demo_df_long_fips)[names(demo_df_long_fips) == "Town"] <- "Town/County"
demo_df_long_fips$County <- NULL

# bind town/state level with county level data
demo_df_long_fips <- rbind(demo_df_long_fips, county_with_fips)

#Add Measure Type and Variable columns
demo_df_long_fips$`Measure Type` <- "Number"
demo_df_long_fips$Variable <- "Demolitions"

#Order and sort columns
demo_df_long_fips <- demo_df_long_fips %>% 
  select(`Town/County`, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(`Town/County`, Year)



