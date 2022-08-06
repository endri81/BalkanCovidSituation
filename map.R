library(leaflet)
library(RColorBrewer)
library(viridisLite)

#Dataframe for map

lat <- c(41.337515, 43.855646, 42.703766, 45.843003, 37.977439, 42.676503, 42.423784, 42.010558, 44.440984, 44.792288, 46.056467 )
long <- c(19.808873, 18.414193, 23.301632, 15.965505, 23.732976, 21.138170, 19.261412, 21.428059, 26.062346, 20.448472, 14.493038)


deathmap <-g7_nots1[c(1:11) , ] %>% 
  mutate(population = population) %>% 
  mutate(deathpermilion = round(deaths_new/population,2))  %>% 
  mutate(lat = lat) %>%
  mutate(long = long)

# Create a color palette with handmade bins.
mybins <- seq(0, 30000, by=1000)




# get domain of numeric data
domain <- range(deathmap$cases_new)

# make palette
mypalette <- colorNumeric(palette = inferno(30), domain = domain)




# Prepare the text for the tooltip:
mytext <- paste(
  "Daily Infections: ", deathmap$cases_new, "<br/>", 
  "Daily Deaths: ", deathmap$deaths_new, "<br/>", 
  "Deaths per milion: ", deathmap$deathpermilion, sep="") %>%
  lapply(htmltools::HTML)



