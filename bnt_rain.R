#Rain Update

library(dplyr)
library(lubridate)
library(plotly)
library(zoo)

#compile list of rain files for the given location

#the Tinicum files all have the name 'tin' in them and end in txt
# tin.files <- list.files("C:/Users/User1/Documents/BNTGWMC/Groundwater data/LennonUpdates/",recursive = T,full.names=T,pattern="tin.*txt")
tin.files <- list.files("C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates",recursive = T,full.names=T,pattern="tin.*txt")



#combine into a single file of precip events
tin.events <- do.call(rbind,lapply(tin.files,read.table,sep="",skip=1,stringsAsFactors=F)) %>%
  mutate(DT = mdy_hms(paste(V1,V2))) %>%
  rename(Count = V3) %>%
  select(DT, Count)

#export, and add to zip file found in bntgroundwater.org/AllData.7z
# write.csv(tin.events,"C:/Users/User1/Documents/BNTGWMC/AllDataWebsite/tin.events.csv",row.names = F)
write.csv(tin.events,"C:/Users/riley/Documents/BNTGWMC/AllDataWebsite/tin.events.csv",row.names = F)


#Also create a daily precip file.  Each event indicates 0.03" of rain
tin.daily <- tin.events %>%
  mutate(Date = as.Date(DT)) %>%
  group_by(Date) %>%
  summarize(rain.inches = 0.03*length(Date))


# write.csv(tin.daily,"C:/Users/User1/Documents/BNTGWMC/AllDataWebsite/tin.daily.csv",row.names = F)
write.csv(tin.daily,"C:/Users/Riley/Documents/BNTGWMC/AllDataWebsite/tin.daily.csv",row.names = F)


#Same process for the high school rain gauge


# HS.files <- list.files("C:/Users/User1/Documents/BNTGWMC/Groundwater data/LennonUpdates/",recursive = T,full.names=T,pattern="HS.*txt")
HS.files <- list.files("C:/Users/Riley/Documents/BNTGWMC/Groundwater data/LennonUpdates/",recursive = T,full.names=T,pattern="HS.*txt")


#combine into a single file of precip events
HS.events <- do.call(rbind,lapply(HS.files,read.table,sep="",skip=1,stringsAsFactors=F)) %>%
  mutate(DT = mdy_hms(paste(V1,V2))) %>%
  rename(Count = V3) %>%
  select(DT, Count)

#export, and add to zip file found in bntgroundwater.org/AllData.7z
# write.csv(HS.events,"C:/Users/User1/Documents/BNTGWMC/AllDataWebsite/HS.events.csv",row.names = F)
write.csv(HS.events,"C:/Users/Riley/Documents/BNTGWMC/AllDataWebsite/HS.events.csv",row.names = F)


#Also create a daily precip file.  Each event indicates 0.1" of rain (Tin was 0.03)
HS.daily <- HS.events %>%
  mutate(Date = as.Date(DT)) %>%
  group_by(Date) %>%
  summarize(rain.inches = 0.1*length(Date))


# write.csv(HS.daily,"C:/Users/User1/Documents/BNTGWMC/AllDataWebsite/HS.daily.csv",row.names = F)
write.csv(HS.daily,"C:/Users/Riley/Documents/BNTGWMC/AllDataWebsite/HS.daily.csv",row.names = F)


#example plot
plot_ly(x=tin.daily$Date,y=tin.daily$rain.inches,type='scatter',mode='markers') %>% layout(title="Rainfall per day in inches")
plot_ly(x=tin.daily$Date,y=tin.daily$rain.inches,type='bar') %>% layout(title="Rainfall per day in inches")

#both

both.daily <- bind_rows(list(tin = tin.daily, HS = HS.daily),.id='source')
plot_ly(x=both.daily$Date,y=both.daily$rain.inches,color=both.daily$source,colors=c("red","blue"),type='scatter',mode='markers') %>% layout(title="Rainfall per day in inches")
plot_ly(x=both.daily$Date,y=both.daily$rain.inches,color=both.daily$source,colors=c("red","blue"),type='bar') %>% layout(title="Rainfall per day in inches")
#rainfall.html, save manually

file.copy("C:/Users/riley/Documents/Coding/BNT/rainfall.html",
          "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/rainfall.html",
          overwrite = T)

#rain by yearmon
a <- tin.daily %>%
  mutate(yearmon = as.yearmon(Date)) %>%
  group_by(yearmon) %>%
  summarize(sum = sum(rain.inches))

saveRDS(a, "rainByYearmon.rds")

plot_ly(x = a$yearmon, y= a$sum, type='bar',text=as.character(a$yearmon))
