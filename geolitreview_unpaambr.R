#code to make a map for range paper
#EVM 12/1/17

#install package to map (leaflet), download data from GoogleSheets (gsheet), save html (htmlwidgets).drawing objects (sp)
install.packages(c("shiny", "leaflet", "gsheet", "htmlwidgets", "sp", "plyr", "dplyr", "magrittr", "ggplot2", "tidyr"))



##get leaflet
library(shiny)
library(ggplot2)
library(leaflet)
library(gsheet)
library(htmlwidgets)
library(sp)
library(magrittr)
library(plyr)
library(tidyr)
library(dplyr)


#import googlesheet (or .csv )with information
url <- 'https://docs.google.com/spreadsheets/d/1eFPE2Lro-KsNijv2yJVZ2WcGYj9hJ-5RkG92T_u5prQ/edit?usp=sharing'
a <- gsheet2text(url)
plantref <- read.csv(text=a) 

#get lats/longs as numbers
lat = as.numeric(as.character(plantref$latitude))
long = as.numeric(as.character(plantref$longitude))

dois = paste0("'","<a"," href", "=", plantref$DOI , ">", "DOI</a>","'") #create hyperlinks from DOIs

#make different markers for unpa and ambr
pal = colorFactor(c("orange", "blue", "purple"), domain = c(2, 1, 3))



#collapse rows into set of data for pop-up- in text citation, species, obs_type, doi
plantref_cols = list(plantref$Citation, plantref$location, plantref$species, dois, sep = " ")
chunks =do.call(paste, plantref_cols)
plantref$chunks = chunks
#aggregate references for those locations
citations = aggregate(plantref$chunks~plantref$latitude, data = plantref, paste, collapse = ';')


#####make data set to map from sheet with citations as chunks####
#find where latitudes line up in plant ref, get longitude
##get longitude indices/indicies to get one of 
longind = match(citations[,1],plantref$latitude)
##get longitude values
longvalues = as.numeric(as.character(plantref$longitude[longind]))
##get latitude vlaues
latvalues = as.numeric(as.character(citations[,1]))
##get label as location
locationlabels = as.character(plantref$location[longind])
##get species at each location
speciesatlocation = aggregate(plantref$species~plantref$latitude, data = plantref, paste, collapse = ',')

###if it has an A, mark as 1, if it has a U, mark as 2, if it has both, mark as 3
####find where species are
As = grep("A", speciesatlocation[,2])
Us = grep("U", speciesatlocation[,2])
Both = match(As, Us)
Bothnum = Us[Both]
Asonly = setdiff(As, Bothnum)
Usonly = setdiff(Us, Bothnum)
###create species vector
Spvec = c()
Spvec[Asonly] = 1
Spvec[Usonly] = 2
Spvec[Bothnum] = 3



#make plant map using lat, long, color marker by species, and include popup with link to paper        
plantmap = leaflet(plantref) %>% addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(lng = longvalues, lat = latvalues,
                   color = ~pal(Spvec),
                   label = locationlabels,
                   popup = citations[,2])

plantmap 

saveWidget(plantmap,file ='plantmap1_9.html', selfcontained = TRUE)  #save the html   


