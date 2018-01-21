
library(rvest)
library(dplyr)

# API changes throughout the day - html from ~3pm had 588 (7 * 84) probability_of_precipitation's, and at 7pm it had 672 (8 * 84 i.e. an extra day's data)
# This tells us we should probably look to use the index (i.e. 0 is today, 3 is in 3 days' time etc)
url <- "ftp://ftp.bom.gov.au/anon/gen/fwo/IDV10753.xml"


html <- read_html(url)
area_1 <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="region"]') %>% html_attr("description")
area_2 <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="public-district"]') %>% html_attr("description")
area_3 <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="location"]') %>% html_attr("description")

max_temp <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_maximum"]') %>% html_text
min_temp <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="air_temperature_minimum"]') %>% html_text
description <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="precis"]') %>% html_text
prob_of_rain <- html_nodes(html, "element") %>% html_nodes(xpath = '//*[@type="probability_of_precipitation"]') %>% html_text


ff <- function() { return(data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), 
                                                   max_temp_0=as.character(), max_temp_1=as.character(), max_temp_2=as.character(), max_temp_3=as.character(), max_temp_4=as.character(), max_temp_5=as.character(), max_temp_6=as.character(), 
                                                   min_temp_0=as.character(), min_temp_1=as.character(), min_temp_2=as.character(), min_temp_3=as.character(), min_temp_4=as.character(), min_temp_5=as.character(), min_temp_6=as.character(), 
                                                   description_0=as.character(), description_1=as.character(), description_2=as.character(), description_3=as.character(), description_4=as.character(), description_5=as.character(), description_6=as.character(), 
                                                   prob_of_rain_0=as.character(), prob_of_rain_1=as.character(), prob_of_rain_2=as.character(), prob_of_rain_3=as.character(), prob_of_rain_4=as.character(), prob_of_rain_5=as.character(), prob_of_rain_6=as.character(), 
                                                   stringsAsFactors = FALSE))
}


data <- ff()
level_1 <- ff()

for (i in 1:length(area_1)) {
  
  level_2 <- ff()
  
  for (j in 1:length(area_2)) {
  
  
  
  }
}







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
















# explore. Why are rows different? 
area_1 %>% length()
area_2 %>% length()
area_3 %>% length()
max_temp %>% length()
min_temp %>% length()
description %>% length()
prob_of_rain %>% length()
