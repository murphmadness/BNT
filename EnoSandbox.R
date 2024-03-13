#Testing graphing some eno data

library(openxlsx)
library(dplyr)
library(plotly)
library(lubridate)


a <- read.xlsx("C:/Users/riley/Documents/BNTGWMC/Groundwater data/Eno/KE010523.xlsx") %>%
  mutate(D.T = as.POSIXct((Date + Time)*24*60*60, origin = "1899-12-30", tz = "GMT")) %>%
  mutate(Feet = -Feet) %>%
  mutate(day.of.week = wday(D.T))

plot_ly(x=a$D.T,y=a$Feet,type='scatter',mode='markers', text = a$day.of.week, color = as.factor(a$day.of.week)) 

#Oct-9 to Nov 7 has jumps that seem somewhat periodic

#Check temp

plot_ly(x=a$D.T,y=a$Temp_F,type='scatter',mode='lines+markers', text = a$day.of.week, color = a$Time) 

#files from email 11/18/23
# Kind Earth Growers are WSLOG001.txt and WSLOG004.txt
# Lodi Hill Well is  WSLOG005.txt,   WSLOG006.txt , and   WSLOG007.txt 

a <- read.table("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw/WSLOG001.txt",
                header = F,
                sep = "\t")

a <- "C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw/WSLOG001.txt"

b <- readChar(a,file.info(a)$size)

a <- read.fwf("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw/WSLOG001.txt",
              widths = c(20,9,3,8,3,6,3,5,3,4,3,4)) %>%
  rename(D.T = V1, Depth = V4, Temp = V6, Voltage = V8) %>%
  mutate(D.T = ymd_hms(D.T),
         Depth = -Depth)

read.eno <- function(x){
  read.fwf(x,
                widths = c(20,9,3,8,3,6,3,5,3,4,3,4)) %>%
    rename(D.T = V1, ID = V2, Depth = V4, Temp = V6, Voltage = V8) %>%
    mutate(D.T = ymd_hms(D.T),
           Depth = -Depth) %>%
    mutate(File.Name = basename(x)) %>%
    mutate(ID = trimws(ID))
}

# a <- read.eno("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw/WSLOG001.txt")

a <- do.call(rbind,lapply(list.files("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw",full.names = T),read.eno))

#Are there separate IDs?
unique(a$ID)

#Files Match with Locations

b <- data.frame(File.Name = c("WSLOG001.txt", "WSLOG003.txt", "WSLOG004.txt" ,"WSLOG005.txt" ,"WSLOG006.txt", "WSLOG007.txt"),
                Location = c("Kind Earth Growers","Well Near PHS","Kind Earth Growers","Lodi Hill Well","Lodi Hill Well","Lodi Hill Well")) %>%
  inner_join(a,by="File.Name")

unique(b %>% select(File.Name,Location,ID))

# File.Name           Location        ID
# 1     WSLOG001.txt Kind Earth Growers   ID 001 
# 6732  WSLOG003.txt      Well Near PHS   ID 003 
# 12181 WSLOG004.txt Kind Earth Growers   ID 001 
# 18508 WSLOG005.txt     Lodi Hill Well   ID 001 
# 25239 WSLOG006.txt     Lodi Hill Well   ID 002 
# 25815 WSLOG007.txt     Lodi Hill Well   ID 002 

#I thought location would match ID, but log 5 doesn't match up.  Check to see if that's correct

plot_ly(x=b$D.T,y=b$Depth,color=b$Location,type='scatter',mode='lines')

plot_ly(x=b$D.T,y=b$Depth,color=b$ID,type='scatter',mode='lines')

#Looks like the IDs do match location and there was an error in the comunication

#Looks like I can determine which is which by the ID
#Create a separate file for the IDs

a <- do.call(rbind,lapply(list.files("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw",full.names = T),read.eno))

b <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoMetaData/EnoLocations.csv") %>%
  inner_join(a,by='ID') %>%
  arrange(D.T)

plot_ly(x=b$D.T,y=b$Depth,color=b$Location,type='scatter',mode='lines')

#try now with the max of every 7 alogrithm I use for the other wells

#But first just check it out as daily max

d <- b %>%
  mutate(date = as.Date(D.T)) %>%
  group_by(date, Location) %>%
  summarise(day.max = max(Depth),day.median = median(Depth),day.min = min(Depth))

plot_ly(x=d$D.T,y=d$day.max,color=d$Location,type='scatter',mode='lines')

#Just plot the daily max against the others despite them using a 7 point max

g <- readRDS("AllWellPlots.rds")

enogaps <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoMetaData/EnoGaps.csv",stringsAsFactors = F) %>%
  mutate(start.gap = mdy_hm(start.gap), end.gap =mdy_hm(end.gap)) %>%
  pivot_longer(contains("gap"),names_to = "maximized",values_to = "D.T") %>%
  mutate(maximized = NA)

e <- d %>%
  rename(Well = Location, D.T = date, maximized = day.max) %>%
  select(Well, D.T, maximized) %>%
  bind_rows(g) %>%
  filter(Well %in% c("Brendas_Way", #just select a few wells for a simplified comparison
                     "Center_Hill",
                     "Dark_Hollow",
                     "Durham_South",
                     "Ervin",
                     "Kind Earth Growers",
                     "Lodi Hill Well",
                     "Mountain_View",
                     "St_Lukes",
                     "Tabor",
                     "Well Near PHS")) %>%
  bind_rows(enogaps) %>%
  arrange(D.T)

plot_ly(x=e$D.T, y=e$maximized,type='scatter',mode='lines',color=e$Well) %>% 
  layout(title="Continuous Monitorring Wells",
         xaxis=list(title="Date"),yaxis=list(title="depth below surface(ft)"))


#Add gaps, then insert in above.
#Plot each well to see where gaps come in

#Something interesting for comparison would be to compare each well across the median.  Let's try that

g <- e %>%
  group_by(Well) %>%
  summarise(median = median(maximized,na.rm=T)) %>%
  inner_join(e, by="Well") %>%
  mutate(Depth.To.Median = maximized-median)

plot_ly(x=g$D.T, y=g$Depth.To.Median,type='scatter',mode='lines',color=g$Well) %>% 
  layout(title="Continuous Monitorring Wells",
         xaxis=list(title="Date"),yaxis=list(title="depth relative to median(ft)"))

#extremes, Center hill big variation, St lukes small variation



########
#Get set up for bimonthly update

#starting with just one file
a <- read.eno("C:/Users/riley/Documents/BNTGWMC/LatestData/EnoRaw/121423CC.txt")

# saveRDS(a,"EnoTest1.rds")

b <- a %>%
  select(D.T, Depth) %>%
  mutate(date = as.Date(D.T)) %>%
  group_by(date) %>%
  summarise(day.max = max(Depth),day.median = median(Depth),day.min = min(Depth))

# saveRDS(b,"Enotest2.rds")

write.csv(b, "C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/Cross_CreekdailyStats.csv")




#2/17/24

#Take a look at Eno Spurious Data

#Daily Stats
a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/Cross_CreekdailyStats.csv") %>%
  mutate(date = ymd(date))

#Unfiltered
b <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/Cross_Creek.csv") %>%
  mutate(D.T = ymd_hms(D.T), type = "Raw")

#Combine and plot
d <- a %>% mutate(Depth = day.max, D.T = as.POSIXct(date), type = "Daily Max") %>%
  select(D.T, Depth,type) %>%
  bind_rows(b) %>%
  filter(D.T > mdy_hms("11/18/23 00:00:00"))

plot_ly(x = d$D.T, y=d$Depth,color=d$type,type='scatter',mode='lines')


#Might be really slow, but try excluding points one at a time based on the rate of change relative to the one before it

#Check on the rates graphically
e <- d %>%
  filter(type == "Raw") %>%
  mutate(rate = 1e4*(lead(Depth) - Depth)/(as.numeric(lead(D.T)) - as.numeric(D.T))) %>%
  mutate(type = "Rate") %>%
  select(D.T,rate,type) %>%
  rename(Depth = rate) %>%
  bind_rows(d)
  
plot_ly(x = e$D.T, y=e$Depth,color=e$type,type='scatter',mode='lines')

#Highest is when there were two measurements a minute apart, although the second seemed correct
#drop right before, -4.8, could have excluded that one

#Try that
#Using data after 11/18/23
p <- b %>% filter(D.T > mdy_hms("11/18/23 00:00:00")) %>%
  mutate(rate = NA)

#initiate
m <- p[1,]
j = 1
for(i in 2:nrow(p)){
  # i = 2
  #calculate rate
  temp1 = 1e4*(p$Depth[i] - m$Depth[j])/(as.numeric(p$D.T[i]) - as.numeric(m$D.T[j]))
  p$rate[i] <- temp1
  if(abs(temp1) < 4){
    m <- bind_rows(m,p[i,])
    j <- j+1
  }
}

p <- p %>%
  select(D.T, rate) %>%
  mutate(type = "rate") %>%
  rename(Depth = rate)

n <- m %>%  mutate(type = "Filtered") %>%
  bind_rows(d,p)

plot_ly(x = n$D.T, y=n$Depth,color=n$type,type='scatter',mode='lines')

#Just for sharing purposes, create an offset

q <- n %>%
  filter(type %in% c("Raw","Filtered")) %>%
  mutate(Depth = ifelse(type == "Raw",Depth,Depth-3)) %>%
  mutate(type = ifelse(type == "Filtered","Filtered (Offset)",type))

plot_ly(x = q$D.T, y=q$Depth,color=q$type,type='scatter',mode='lines') %>%
  layout(title="Raw vs Filtered Data shown offset",
         xaxis=list(title="Date"),yaxis=list(title="depth of Raw data"))

  

