#Sandbox

library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)

#8/31/22
#plot two graphs Bedminster and Brendas way
#One looks much noiser
#graph them compared

a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/Bedminster.csv",stringsAsFactors = F) %>%
  mutate(D.T = ymd_hms(D.T)) %>%
  mutate(well="Bedminster") %>% # add offset just so I can look at them close to each other
  mutate(F.Offset = F.Offset - 80)

b <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R NewNames/Brendas_Way.csv",stringsAsFactors = F) %>%
  mutate(D.T = ymd_hms(D.T)) %>%
  mutate(well="Brendas_Way") %>%
  bind_rows(a) %>%
  filter(D.T > "2019-01-01",D.T < "2019-06-01"  ) #just Jan 2019 because it's too slow otherwise


plot_ly(x=b$D.T,y=b$F.Offset,type='scatter',mode='lines+markers', color = b$well)


#I don't think Brenda's way is actually an active well. Maybe that's why it doesn't lok as noisy.  Still, Bedminster looks extra noisy

#Or just daily stats for approx

a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/BedminsterdailyStats.csv",stringsAsFactors = F) %>%
  mutate(date = ymd(date)) %>%
  mutate(well="Bedminster") %>%
  mutate(day.max = day.max - 80)

b <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R byDay/Brendas_WaydailyStats.csv",stringsAsFactors = F) %>%
  mutate(date = ymd(date)) %>%
  mutate(well="Brendas_Way") %>%
  bind_rows(a)


plot_ly(x=b$date,y=b$day.max,type='scatter',mode='lines', color = b$well,text=wday(b$date,label=T))

#Wow it's really a big fluctuation on a somewhat monthly level . . . but looks like noise because of the long time period


#9/9/10

#plot volts vs time
#latest alex reading
a <- read.csv("C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates/2022_08/Alex_220816.csv",skip=1,stringsAsFactors = F) %>%
  mutate(D.T = mdy_hms(paste(Date,Time,sep=" ")))

plot_ly(x=a$D.T,y=a$Volts,type='scatter',mode='markers')


#latest rapp reading
a <- read.csv("C:/Users/riley/Documents/BNTGWMC/Groundwater data/LennonUpdates/2022_08/Rapp_220816.csv",skip=1,stringsAsFactors = F) %>%
  mutate(D.T = mdy_hms(paste(Date,Time,sep=" ")))

plot_ly(x=a$D.T,y=a$uS,type='scatter',mode='markers')

#9/11/22
#Convert filepath string to the better format

convert_clip <- function(){gsub("\\","/",readClipboard())} #doesn't work
gsub("\\\\","/",readClipboard())

gsub('"',"",  gsub("\\\\","/",readClipboard()))

read.csv(gsub('"',"",  gsub("\\\\","/",readClipboard())))

cc <- function(){gsub('"',"",  gsub("\\\\","/",readClipboard()))}
cc()
#spits out "C:/Users/riley/Documents/BNTGWMC/LatestData/Roffset/WellNames.csv", nice

#11/16/22 I'd like to make some graphs that have the gaps in them.

#Let's start one at a time
#See if I can use the filtered version to make it easier

a <- readRDS("C:/Users/riley/Documents/BNTGWMC/LatestData/R Lists/Alex.rds")[[1]] %>%
  select(D.T, maximized) %>%
  filter(complete.cases(maximized))

#might come back to that, but for now let's just use the summary plot

a <- read.csv("C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated/Center_Hill_Summary.csv", stringsAsFactors = F) %>%
  select(D.T, maximized) %>%
  mutate(D.T = ymd_hms(D.T)) %>%
  filter(complete.cases(maximized))

plot_ly(x=a$D.T, y=a$maximized,type='scatter',mode='lines')



#Put a gap in between:
#"2011-06-21"
#"2011-10-11"

b <- a %>%
  filter(!(D.T > as.Date("2011-06-21") & D.T < as.Date("2011-10-11"))) %>%
  bind_rows(data.frame(D.T = c(ymd("2011-06-21"),ymd("2011-10-11")),maximized = c(NA,NA))) %>%
  arrange(D.T) %>%
  mutate(Well = "Center_Hill")

plot_ly(x=b$D.T, y=b$maximized,type='scatter',mode='lines')

#looks good.  Now how to do more than one

#have this in csv later
gaps <- data.frame(start.gap = ymd(c("2011-06-21","2021-12-24")),end.gap = ymd(c("2011-10-11","2022-05-14")),Well = "Center_Hill")

d <- a %>%
  mutate(Well ="Center_Hill") %>%
  inner_join(gaps,by="Well") %>%
  mutate(test = ifelse((as.numeric(as.Date(D.T)) > as.numeric(start.gap) & as.numeric(as.Date(D.T)) < as.numeric(end.gap)),1,0)) %>%
  group_by(D.T,maximized,Well) %>%
  summarize(max = max(test)) %>%
  ungroup() %>%
  filter(max == 0) %>%
  select(-max)
  
  # filter(!(D.T > start.gap & D.T < end.gap))


plot_ly(x=d$D.T, y=d$maximized,type='scatter',mode='lines')

#that seemed a little complicated, but it worked.  Now insert NA dates

e <- gaps %>%
  pivot_longer(contains("gap"),names_to = "maximized",values_to = "D.T") %>%
  mutate(maximized = NA) %>%
  bind_rows(d) %>%
  arrange(D.T)
  
plot_ly(x=e$D.T, y=e$maximized,type='scatter',mode='lines')

#Nice.  Now do it for all the summarized data

#file names of all of the plots that have just 1000 points
a <- "C:/Users/riley/Documents/BNTGWMC/LatestData/R truncated"
b <- list.files(a,pattern="csv")

#gap data
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

# d <- f1(b[1])

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

plot_ly(x=e$D.T, y=e$maximized,type='scatter',mode='lines',color=e$Well) %>% 
  layout(title="Continuous Monitorring Wells",
         xaxis=list(title="Date"),yaxis=list(title="depth below surface(ft)"))

#AllWells2022.html