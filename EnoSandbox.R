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
