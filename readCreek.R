#Read Rapp Creek

library(dplyr)
library(plotly)
library(lubridate)

rapp.files <- c(list.files("C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates",recursive = T,full.names=T,pattern="Moss",ignore.case = T),
                list.files("C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates",recursive = T,full.names=T,pattern="Rapp",ignore.case = T))
                
a <- do.call(bind_rows,lapply(rapp.files,function(x){
  read.csv(x,skip=1)
})) %>%
  mutate(D.T = mdy_hms(paste(Date, Time))) %>%
  arrange(D.T) %>%
  distinct() %>%
  filter(D.T > "2000-01-01")
  

plot_ly(x=a$D.T, y=a$uS, type='scatter',mode='lines') %>%
  layout(title="Rapp Creek Conductivity",xaxis=list(title="Time"),yaxis=list(title="microsemens"))
#Rapp.html

#remove bad data

#What are these jumps? Like Dec6th 2022.  Is this quarry discharge?
#Why did the noise leave around 11/15 2022 