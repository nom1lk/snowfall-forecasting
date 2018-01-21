require(rvest)

text <- '<div class="style"> \n <input id="a" value="123">'


text <- '<div class="style">   <input id="a" value="123">   <input id="b"></div>'

h <- read_html(text)

h %>% 
  html_nodes(xpath = '//*[@id="a"]') %>%
  xml_attr("value")












# let's try a loop 

data <- data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), max_temp=as.character(), min_temp=as.character(), description=as.character(), prob_of_rain=as.character(), stringsAsFactors = FALSE)
level_1 <- data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), max_temp=as.character(), min_temp=as.character(), description=as.character(), prob_of_rain=as.character(), stringsAsFactors = FALSE)
for (i in 1:length(area_1)) {
  
  level_2 <- data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), max_temp=as.character(), min_temp=as.character(), description=as.character(), prob_of_rain=as.character(), stringsAsFactors = FALSE)
  for (j in 1:length(area_2)) {
    
    level_3 <- data.frame(area_1=as.character(), area_2=as.character(), area_3=as.character(), max_temp=as.character(), min_temp=as.character(), description=as.character(), prob_of_rain=as.character(), stringsAsFactors = FALSE)
    for (k in 1:length(area_3)) {
      tryCatch( level_3[k, 'area_1'] <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="region"]') %>% html_attr("description"), error = function(e) { level_3[k, 'area_1'] <- NA_character_ })
      tryCatch( level_3[k, 'area_2'] <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="public-district"]') %>% html_attr("description"), error = function(e) { level_3[k, 'area_2'] <- NA_character_ })
      tryCatch( level_3[k, 'area_3'] <- html_nodes(html, "area") %>% html_nodes(xpath = '//*[@type="location"]') %>% html_attr("description"), error = function(e) { level_3[k, 'area_3'] <- NA_character_ })
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




