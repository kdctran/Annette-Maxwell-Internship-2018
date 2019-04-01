# https://jasminedumas.shinyapps.io/Choropleth_Zipcodes/

install.packages("rgdal")
install.packages("leaflet")
install.packages("magrittr")

library(rgdal)
library(leaflet)
library(dplyr)
library(magrittr)
library(readr)
library(knitr)
library(tidyselect)

# USA level zipcode for 2015
tmp2 = tempdir()

url2 = "http://www2.census.gov/geo/tiger/GENZ2015/shp/cb_2015_us_zcta510_500k.zip"

file <- basename(url2)

download.file(url2, file)

unzip(file, exdir = tmp2)

usa <- readOGR(dsn = tmp2, layer = "cb_2015_us_zcta510_500k", encoding = "UTF-8")

dim(usa)
class(usa) # the human data is located at usa@data

# change the column name of the usa@data "ZCTA5CE10" to zipcode

names(usa)[1] = "zipcode"


# url3 = "http://www.unitedstateszipcodes.org/zip_code_database.csv"
# 
# file3 <- basename(url3)
# 
# # will be located in the project directory folder
# 
# download.file(url3, file3) 

all_usa_zip <- read_csv("C:/Users/khtran/Documents/CHURCH CHA ADHERENCE/map/zip_code_database.csv")

# filter down to the specific state and select 

specific_state <- all_usa_zip %>% dplyr::filter(state == "CA") %>% 
  select(zip, primary_city, county, irs_estimated_population_2015)

# change the column names for the merge by arguement

colnames(specific_state) <- c("zipcode", "City", "County", "estimated_population")

# change the zipcode to a factor to full join the table

specific_state$zipcode = factor(specific_state$zipcode)

# remove redundant County label in County column

specific_state$County <- gsub("County", "", specific_state$County) 
# there is probably a space after now but not concerned about that

# full join the data set 

state_join <- dplyr::full_join(usa@data, specific_state)

# remove rows with NA's - i.e. remove everything except the choosen state

state_clean = na.omit(state_join)

# Merge a Spatial object having a data.frame (i.e. merging of non-spatial attributes).
# all.x = F removes NA values that are not common to both datasets

STATE_SHP <- sp::merge(x=usa, y=state_clean, all.x = F)

head(STATE_SHP@data)
dim(STATE_SHP)
# color palette
# pal <- colorNumeric(
#   palette = "Blues",
#   domain = STATE_SHP$estimated_population
# )
pal <- colorBin(palette = "BuPu", domain = STATE_SHP()$estimated_population, bins = 8)

# pop values
state_popup <- paste0("County: ", 
                      STATE_SHP$County, 
                      "City: ", 
                      STATE_SHP$City, 
                      "Est. Population: ",
                      STATE_SHP$estimated_population)
# plot the map
leaflet(data = STATE_SHP) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillOpacity = 0.7, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) 
