
library(rvest)
library(dplyr)

# http://www.bom.gov.au/catalogue/data-feeds.shtml#product
# ftp://ftp.bom.gov.au/anon/gen/fwo/IDV10753.xml


# API changes throughout the day - html from ~3pm had 588 (7 * 84) probability_of_precipitation's, and at 7pm it had 672 (8 * 84 i.e. an extra day's data)
# This tells us we should probably look to use the index (i.e. 0 is today, 3 is in 3 days' time etc)
url <- "ftp://ftp.bom.gov.au/anon/gen/fwo/IDV10753.xml"


html <- read_html(url)
area_1_html <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="region"]') 
area_1 <- area_1_html %>% html_nodes(xpath = '//*[@type="region"]') %>% html_attr("description")
                       
area_2_html <- area_1_html %>% html_nodes(xpath = '//*[@type="public-district"]')
area_2_aac <- area_2_html %>% html_attr("aac") 
area_2 <- area_2_html %>% html_attr("description")
area_2_parent_aac <- area_2_html %>% html_attr("parent-aac")

area_3_html <- area_2_html %>% html_nodes(xpath = '//*[@type="location"]') 
area_3_aac <- area_3_html %>% html_attr("aac")
area_3 <- area_3_html %>% html_attr("description")
area_3_parent_aac <- area_3_html %>% html_attr("parent-aac")

area_2_table <- cbind(area_2_aac, area_2, area_2_parent_aac) %>% as.data.frame()
area_3_table <- cbind(area_3_aac, area_3, area_3_parent_aac) %>% as.data.frame()

# table of area_1, area_2, and area_3
area_table <- left_join(area_3_table, area_2_table, by=c("area_3_parent_aac" = "area_2_aac"))
area_table[] <- lapply(area_table, as.character)
area_table

ff <- function() { return(data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), 
                                     max_temp_0=as.character(), max_temp_1=as.character(), max_temp_2=as.character(), max_temp_3=as.character(), max_temp_4=as.character(), max_temp_5=as.character(), max_temp_6=as.character(), max_temp_7=as.character(),  
                                     min_temp_0=as.character(), min_temp_1=as.character(), min_temp_2=as.character(), min_temp_3=as.character(), min_temp_4=as.character(), min_temp_5=as.character(), min_temp_6=as.character(), min_temp_7=as.character(), 
                                     description_0=as.character(), description_1=as.character(), description_2=as.character(), description_3=as.character(), description_4=as.character(), description_5=as.character(), description_6=as.character(), description_7=as.character(), 
                                     prob_of_rain_0=as.character(), prob_of_rain_1=as.character(), prob_of_rain_2=as.character(), prob_of_rain_3=as.character(), prob_of_rain_4=as.character(), prob_of_rain_5=as.character(), prob_of_rain_6=as.character(), prob_of_rain_7=as.character(),
                                     stringsAsFactors = FALSE))
}



# what do we want to do now? 
# we can easily get the area_1, area_2 and area_3 for each area_3, but what else do we need and how will we get it? 
# For each area_3, we need max_temp, min_temp, description, and prob_of_rain

# length(area_table$area_3_aac)
data <- ff()
for (i in 1:length(area_table$area_3_aac)) {      

      data[i, 'area_1'] <- "Victoria"
      data[i, 'area_2'] <- area_table[area_table$area_3_aac == area_table$area_3_aac[i], 'area_2']
      data[i, 'area_3'] <- area_table[area_table$area_3_aac == area_table$area_3_aac[i], 'area_3']
      
}  

data[,c(1:3)]
head(data)



# these work but we can't associate the values with an index
max_temp <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_maximum"]') %>% html_text
min_temp <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_minimum"]') %>% html_text
description <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="precis"]') %>% html_text
prob_of_rain <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="probability_of_precipitation"]') %>% html_text


# explore.
area_1 %>% length()
area_2 %>% length()
area_3 %>% length()
max_temp %>% length()
min_temp %>% length()
description %>% length()
prob_of_rain %>% length()



area_containers_html <- html %>% html_nodes("area") %>% html_nodes(xpath = '//*[@type="location"]')


library(RCurl)
library(XML)

rc <- getURL(url) %>% htmlParse(asText=TRUE)
r <- xmlToList(rc)


# htmlParse(getURL(URL),asText=TRUE)

data <- getURL("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML") %>% htmlParse()
data <- xmlParse("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML")

xml_data <- xmlToList(data)

location <- as.list(xml_data[["data"]][["location"]][["point"]])

start_time <- unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])


data <- xmlParse(getURL("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML"))

########
time_path <- "//start-valid-time"
temp_path <- "//temperature[@type='hourly']/value"

df <- data.frame(
  latitude=data[["number(//point/@latitude)"]],
  longitude=data[["number(//point/@longitude)"]],
  start_valid_time=sapply(data[time_path], xmlValue),
  hourly_temperature=as.integer(sapply(data[temp_path], as, "integer")))



library(xml2)
data <- read_xml("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML")

# Point locations
point <- data %>% xml_find_all("//point")
point %>% xml_attr("latitude") %>% as.numeric()
point %>% xml_attr("longitude") %>% as.numeric()

# Start time
data %>% 
  xml_find_all("//start-valid-time") %>% 
  xml_text()

# Temperature
data %>% 
  xml_find_all("//temperature[@type='hourly']/value") %>% 
  xml_text() %>% 
  as.integer()






for (i in length(area_containers_html)) { 
  forecasts <- area_containers_html[i] %>% 
  for (j in length())
  
  
  data[i, 'max_temp_0'] <-    
    }



for (i in 1:length(area_table$area_3_aac)) {
      
      data[i, 'max_temp_0'] <- 
      data[i, 'max_temp_1'] <-  
      data[i, 'max_temp_2'] <-  
      data[i, 'max_temp_3'] <-  
      data[i, 'max_temp_4'] <-  
      data[i, 'max_temp_5'] <-  
      data[i, 'max_temp_6'] <-  
      data[i, 'max_temp_7'] <-
      data[i, 'min_temp_0'] <-  
      data[i, 'min_temp_1'] <-  
      data[i, 'min_temp_2'] <-  
      data[i, 'min_temp_3'] <-  
      data[i, 'min_temp_4'] <-  
      data[i, 'min_temp_5'] <-  
      data[i, 'min_temp_6'] <-
      data[i, 'min_temp_7'] <-
      data[i, 'description_0'] <-  
      data[i, 'description_1'] <-  
      data[i, 'description_2'] <-  
      data[i, 'description_3'] <-  
      data[i, 'description_4'] <-  
      data[i, 'description_5'] <-  
      data[i, 'description_6'] <-
      data[i, 'description_7'] <-
      data[i, 'prob_of_rain_0'] <-  
      data[i, 'prob_of_rain_1'] <-  
      data[i, 'prob_of_rain_2'] <-  
      data[i, 'prob_of_rain_3'] <-  
      data[i, 'prob_of_rain_4'] <-  
      data[i, 'prob_of_rain_5'] <-  
      data[i, 'prob_of_rain_6'] <- 
      data[i, 'prob_of_rain_7'] <-       

}        
        
              
  tryCatch(  , error = function(e) {       } )    
      
    }
    
    





objects <- c("area_1", "area_2", "area_2_aac", "area_2_parent_aac", "area_3", "area_3_aac", "area_3_parent_aac") 

for (i in 1:length(objects)) { assign(objects[i], as.character(get(objects[i]))) }
for (i in 1:length(objects)) { assign(objects[i], as.data.frame(get(objects[i]))) }
for (i in 1:length(objects)) { assign(colnames(get(objects[i])), objects[i]) }













data <- ff()

level_1 <- ff()

for (i in 1:length(area_1)) {
  
  level_2 <- ff()
  
  for (j in 1:length(area_2)) {
    
    level_3 <- ff()
    
    for (k in 1:length(area_3)) {
      tryCatch( level_3[k, 'area_1'] <- area_1[i], error = function(e) { level_3[k, 'area_1'] <- NA_character_ })
      tryCatch( level_3[k, 'area_2'] <- area_2[j], error = function(e) { level_3[k, 'area_2'] <- NA_character_ })
      tryCatch( level_3[k, 'area_3'] <- area_3[k], error = function(e) { level_3[k, 'area_3'] <- NA_character_ })
      tryCatch( level_3[k, 'max_temp'] <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_maximum"]') %>% html_text, error = function(e) { level_3[k, 'max_temp'] <- NA_character_ })
      tryCatch( level_3[k, 'min_temp'] <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_minimum"]') %>% html_text, error = function(e) { level_3[k, 'min_temp'] <- NA_character_ })
      tryCatch( level_3[k, 'description'] <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="precis"]') %>% html_text, error = function(e) { level_3[k, 'description'] <- NA_character_ })
      tryCatch( level_3[k, 'prob_of_rain'] <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="probability_of_precipitation"]') %>% html_text, error = function(e) { level_3[k, 'prob_of_rain'] <- NA_character_ })
      
    }
    level_2 <- rbind(level_2, level_3)
    
  }
  level_1 <- rbind(level_1, level_2)
  
}
data <- rbind(data, level_1)



# for each a3 in each a2 in each a1, we want to do the following
# generate an entry to a dataframe consisting of:
# max_temp_i, min_temp_i, description_i, prob_of_rain_i, for i in 0 - 6 where zero is today and 6 is 6 days from now
# 
















