#Bimonthyl Eno
#Run this script before the bimonthly update

library(dplyr)
library(lubridate)
library(data.table)

#For now have list of which file goes with which name.  Hopefully this will go by the ID field once things are better established

#File names and associated results
a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoMetaData/EnoLocationsByFileName.csv",stringsAsFactors = F)

#Read all files into one data.frame and join with the names
#note read.eno is defined in EnoSandbox.R
b <- do.call(rbind,lapply(list.files("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw",full.names = T),read.eno)) %>%
  inner_join(a,by="File.Name")
  
#work up in the same way, same filters as the global water loggers
d <- b %>%
  select(D.T, Depth, Location) %>%
  distinct() %>%
  arrange(D.T)

#Break into invividual locations and create a file for each

e <- unique(d$Location)

# for(i in 1:length(e)){
  i= 1 #remove
  
  g <- d %>%
    filter(Location == e[i]) %>%
    mutate(maximized = frollapply(Depth,7,FUN=max,fill=NA,align="center"))
  
  #A separate file will contain daily max mins and medians.  Calculate this in p as 'wellname'dailyStats.csv
  p <- g %>%
    mutate(date = as.Date(D.T)) %>%
    group_by(date) %>%
    summarise(day.max = max(Depth),day.median = median(Depth),day.min = min(Depth))
  
  #One file used for graphing or quick work-ups is just 1000 points equally spaced as 'wellname'_Summary.csv
  r <- g[seq(1,nrow(g),floor(nrow(g) / 1000)),]
  
  #File with 1000 points
  write.csv(r,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated/",e[i],"_Summary.csv",sep=""),row.names = F)
  
  #Filtered data is uploaded as it's own file.  This is used for interactive graphing on the website
  s <- data.frame(g$D.T, g$maximized)
  names(s) <- c("D-T","Feet")
  write.csv(s,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/",e[i],"_Filtered.csv",sep=""),row.names = F)
  
  #Full file
  write.csv(g,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/",e[i],".csv",sep=""),row.names = F)
  
  #by day file
  write.csv(p,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/",e[i],"dailyStats",".csv",sep=""),row.names = F)
  
  #Don't bother with rds for now
  # #save the fullFile and summary as rds.  This will be used next time to append new data
  # saveRDS(list(q,r),g$rdsPath[i])
  
# }


  
  
  
  
  
# plot_ly(x=g$D.T,y=g$Depth,type='scatter',mode='lines') %>%
#   add_trace(x=g$D.T,y=g$maximized)
# #Consider a little more elegant way of removing data when well kicks on
# #something like that it can't change by more than a given threshold within a set time period
# #will need some tests to see if it works
# #might still want some averaging after that




