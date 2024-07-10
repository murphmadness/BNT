

library(plotly)
library(lubridate)
library(dplyr)
library(data.table)
library(tidyr)

#Every two months data from a set of well depth loggers are downloaded.
#This script appends the new data to the previously processed data
#The date is filtered several ways, plots are created, 
#and some data is ultimately uploaded to the bntgroundwater website

#v3 expects there to be some extra eno files for the plt each well portion and beyond


#######################
#######################  Monthly Update
#######################
#######################
#######################
#######################

#########
#########The date modified needs to be defined for the new files
#########with the EXACT date, not a date from before
#########
b <- as.Date("2024-06-20")

#h defines the location of all the raw well files including the new files
h <- "C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates/wells only"

#list all files in a
a <- list.files(h,full.names=F)


#Subset the new file paths as e
d <- as.Date(file.mtime(paste(h,a,sep="/")))
e <- a[d == b]

#this file differentiates the old and new naming conventions for the wells.  By owner or by street name
g <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/Roffset/WellNames.csv")

#This function is a quick check to make sure the correct number of files matches up
f1 <- function(x){
  temp1 <- grep(x,e,ignore.case = T)
  if(length(temp1) > 0){
    return(e[temp1])
  }else{
    return("none")
  }
}

g$newFile <- unlist(lapply(g$Owner,f1))


#subset with just with new files
g <- g[-grep("none",g$newFile),]

#Prints the rows of g and the length of e, these should be equal if all was done correctly
nrow(g)
length(e)


#The filepath with the previous data that will be appended with the new data
q <- "C:/Users/riley/Documents/BNTGWMC/LatestData/R Lists/"

#match up the filepath of the old and new files
g$rdsPath <- paste(q,g$Owner,".rds",sep="")
g$newFile <- paste(h,g$newFile,sep="/")

#This loop will process each file

for(i in 1:nrow(g)){

  #read the new file which was saved as rds file type.  It is a list with two data frames in it
  #the first item in the list is the full data file while the second item is an abbreviated data.frame which will be remade
  n <- readRDS(g$rdsPath[i])
  
  #Use only the first item of the list
  n <- n[[1]]
  
  #read the new csv file
  p <- read.csv(g$newFile[i],skip=1)
  
  #The first three headers should be Date, Time and Feet, take only those ignoring Volts etc.
  p <- p[,1:3]
  
  #Convert the Date and Time to Date-Time as D.T.  Then subset and rename to contain only D.T and Feet
  p[,1] <- paste(as.character(p[,1]),as.character(p[,2]),sep=" ")
  p <- p[,c(1,3)]
  names(p) <- c("D.T","Feet")
  
  #Convert from string to posixct
  p$D.T <- mdy_hms(p$D.T)
  
  #Using just the D.T and Feet columns combine the old and new files
  n <- n[,1:2]
  n <- rbind(n,p)
  
  #Order by date
  n <- n[order(n$D.T),]
  
  #Remove duplicates
  n <- unique(n)
  
  #rename as m for convenience of pasting older code
  m <- n
  
  
  
  #All offset data is located in this file.  This is what adjusts the raw data (feet of sensor below water level)
  #to the functional value of feet below the surface
  #Each offset value is used from the date specified on until a new date is specified for that well
  a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/Roffset/Roffset.csv")
  
  #Converts to DateTime and subset to only use the well being updated for each round of the loop
  a[,1] <- paste(as.character(a[,1]),as.character(a[,2]),sep=" ")
  a <- a[,c(1,3,4)]
  names(a)[1] <- "D.T"
  a$D.T <- mdy_hms(a$D.T)
  b <- a[grep(g$Owner[i],a$Well),]
  
  
  #First apply the first offset value to the entire data.frame (in case there is data before the first offset)  
  m$Offset <- b$offset[1]
  #Then loop through each offset value applying it to the offset column for all time periods after the date-time specified
  for(j in 1:nrow(b)){
    m$Offset[m$D.T > b$D.T[j]] <- b$offset[j]
    
  }
  
  #Continue with processing the data several ways
  #note that some code has been updated to use dplyr instead of base r functions
  
  #Define F.Offset as Feet + Offset to get to the depth below the surface
  #make maximized the max point out of every 7 to try to scrub out points where the pump has kicked on
  q <- m %>%
    mutate(F.Offset = Feet + Offset) %>%
    mutate(maximized = frollapply(F.Offset,7,FUN=max,fill=NA,align="center")) %>%
    filter(complete.cases(F.Offset))
  
  #A separate file will contain daily max mins and medians.  Calculate this in p as 'wellname'dailyStats.csv
  p <- q %>%
    mutate(date = as.Date(D.T)) %>%
    group_by(date) %>%
    summarise(day.max = max(F.Offset),day.median = median(F.Offset),day.min = min(F.Offset))
  
  #One file used for graphing or quick work-ups is just 1000 points equally spaced as 'wellname'_Summary.csv
  r <- q[seq(1,nrow(q),floor(nrow(q) / 1000)),]

  #File with 1000 points
  write.csv(r,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated/",g$Street[i],"_Summary.csv",sep=""),row.names = F)
  
  #Filtered data is uploaded as it's own file.  This is used for interactive graphing on the website
  s <- data.frame(q$D.T, q$maximized)
  names(s) <- c("D-T","Feet")
  write.csv(s,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/",g$Street[i],"_Filtered.csv",sep=""),row.names = F)
  
  #Full file
  write.csv(q,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/",g$Street[i],".csv",sep=""),row.names = F)
  
  #by day file
  write.csv(p,paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/",g$Street[i],"dailyStats",".csv",sep=""),row.names = F)
  
  #save the fullFile and summary as rds.  This will be used next time to append new data
  saveRDS(list(q,r),g$rdsPath[i])
}

#This serves as a quick check that the dates updated are new.  Check that the dates make sense once they're printed to the terminal

f2 <- function(x){ 
  temp1 <- read.csv(paste("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/",x,".csv",sep=""))
  
  return(temp1$D.T[nrow(temp1)])
}

unlist(lapply(g$Street,f2))

#######################
#######################  End Monthly Update
#######################  For strict data processing.  Continue for thumbnail graphs and creating the zip file
#######################
#######################
#######################


#######################
#######################  Plot of Each Well
#######################
#######################
####################### needs to include eno plot.  Not added as of 12/26/23
#######################

#Another convenient quick check is making a thumbnail plot of each well with will be viewed at
#http://bntgroundwater.org/QuickGraph.html

library(ggplot2)

#the lists of data
a <- list.files("C:/Users/riley/Documents/BNTGWMC/LatestData/R Lists",full.names = T,pattern='rds')

#Use this file to tie the surnames to the street names by convention
b <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/Roffset/WellNames.csv")
#just the surnames from a
d <- substr(a,nchar("C:/Users/riley/Documents/BNTGWMC/LatestData/R Lists")+2,nchar(a)-4)

#loop thorugh and make a graph of each
for(i in 1:length(a)){

  e <- readRDS(a[i])
  e <- e[[1]]
  
  Street.Name <- b$Street[tolower(b$Owner) == tolower(d[i])]
  Street.Name
  jpeg(paste("C:/Users/riley/Documents/BNTGWMC/LatestData/Rgraphs/",Street.Name,".jpg",sep=""))
  print(ggplot(e,aes(x=D.T,y=maximized)) + geom_line()+xlab("Date") + ylab("Filtered Well Data") + 
    labs(title=Street.Name) +
    theme(text = element_text(size=30)))
  dev.off()
}


#######################
#######################  End Plot of Each Well
#######################
#######################
#######################
#######################


##############
##############
############## zipit
##############
##############

#archive old zip file by renaming it then start new one
#This will be posted to the site once compressed

system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/riley/Documents/BNTGWMC/LatestData/readMe.txt"',
       show.output.on.console = T)

system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/riley/Documents/BNTGWMC/LatestData/LoggerNotes.txt"',
       show.output.on.console = T)

system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/*.csv"',
       show.output.on.console = T)

system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated/*.csv"',
       show.output.on.console = T)

system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/*.csv"',
       show.output.on.console = T)

#Files from bnt_rain.R
system('7z a "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllData.7z" "C:/Users/Riley/Documents/BNTGWMC/AllDataWebsite/*.csv"',
       show.output.on.console = T)




###Now upload Alldata and filtered and graphs


##############
##############
############## end zipit
##############
##############

#Then files are uploaded to the website


##############
##############
############## AllPlots
##############
##############

library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)
library(reticulate)

a <- "C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated"
b <- list.files(a,pattern="csv")

#gap data will bee used to slice out bad data for viewing
gaps <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/Roffset/gaps.csv",stringsAsFactors = F) %>%
  mutate(start.gap = mdy_hm(start.gap), end.gap =mdy_hm(end.gap))


#Make a data.frame for each that contains the well names

f1 <- function(x){
  #get the name of just the well
  temp1 <- gsub("\\_Summary\\.csv","",x)
  temp2 <- read.csv(paste(a,x,sep="/"),stringsAsFactors = F) %>%
    select(D.T, maximized) %>%
    mutate(D.T = ymd_hms(D.T)) %>%
    filter(complete.cases(maximized)) %>%
    mutate(Well = temp1)
  return(temp2)
}


d <- do.call("rbind",lapply(b,f1)) %>%
  inner_join(gaps,by="Well") %>%
  mutate(test = ifelse((as.numeric(D.T) > as.numeric(start.gap) & as.numeric(D.T) < as.numeric(end.gap)),1,0)) %>%
  group_by(D.T,maximized,Well) %>%
  summarize(max = max(test)) %>%
  ungroup() %>%
  filter(max == 0) %>%
  select(-max)

e <- gaps %>%
  pivot_longer(contains("gap"),names_to = "maximized",values_to = "D.T") %>%
  mutate(maximized = NA) %>%
  bind_rows(d) %>%
  arrange(D.T)


# ####12/14/23 with eno temporary
# 
# e <- readRDS("Enotest2.rds") %>%
#   mutate(Well = "Cross_Creek", D.T = as.POSIXct(date),maximized=day.max) %>%
#   select(Well, maximized, D.T) %>%
#   bind_rows(e)
  


# #Save for comparing in Eno Sandbox
# saveRDS(e,"AllWellPlots.rds")

#Save over file 
#AllWells2022.html and .png #now 2023
#AllWells2023.html
#automate in script later
plot_ly(x=e$D.T, y=e$maximized,type='scatter',mode='lines',color=e$Well) %>% 
  layout(title="Continuous Monitorring Wells",
         xaxis=list(title="Date"),yaxis=list(title="depth below surface(ft)"))


file.copy("C:/Users/riley/Documents/Coding/BNT/AllWells2023.html",
          "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllWells2023.html",
          overwrite = T)

file.copy("C:/Users/riley/Documents/Coding/BNT/AllWells2023.png",
          "C:/Users/riley/Documents/BNTGWMC/LatestData/R Filtered/AllWells2023.png",
          overwrite = T)


# #just for Earthday
# # e <- e %>%
# #   filter(Well != "Palisades_High",
# #          Well != "Gruver_3")
# 
# e <- e %>%
#   filter(         Well != "Gruver_3")
# 
# #"C:\Users\riley\Pictures\Misc 2023\BNT2023.png"

##############
##############
############## end AllPlots
##############
##############

