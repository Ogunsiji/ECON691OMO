#This is a script for Assignment 2 Submitted to Prof Groves
#Created by Ogunsiji M.O. on September 21, 2021


rm(list=ls())

library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(tidycensus)
library(stringr)
library(ggplot2)
library(sf)
library(cowplot)

# (1)
states = c("south-dakota", "north-dakota","montana","minnesota","nebraska")
for (i in states){
  url = paste0("https://www.nytimes.com/elections/2016/results/",i)
  webpage = read_html(url)
  tables = webpage %>%
    html_nodes("table")
  results = as.data.frame(html_table(tables[2], header=TRUE, fill=TRUE))
  
  temp = results %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
           "Trump" = as.numeric(gsub(",","", Trump)),
           "pctClinton" = (Clinton)/(Clinton + Trump),
           "pctTrump" = (Trump)/(Clinton + Trump),
           "state" = i)
  
  assign(i, temp)
}

votes = rbind(minnesota,`north-dakota`,`south-dakota`, nebraska, montana)

# Save the data
save(votes, file="./Output/votes.RData")

# (2)
library(tidycensus)
#Pre-defining variables to be used in loop
vars = c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003",
         "B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033",
         "B07001_049","B07001_065","B07001_081" )

fips = c(27,38,46,31,30)

# API Command
k = 1

# For 2016 census
for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2016,
               geometry = TRUE)
  temp = acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value = estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area = st_area(temp)
  map = temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map"),map)
  
  k = k+1
  rm(temp, map)
}

CENSUS.1=rbind(minnesotacensus,`north-dakotacensus`,`south-dakotacensus`,
               nebraskacensus,montanacensus)
States=rbind(minnesotamap,`north-dakotamap`,`south-dakotamap`,nebraskamap,
             montanamap)

CENSUS.1$NAME=as.data.frame(str_split_fixed(CENSUS.1$NAME, ",", 2))[,1]
CENSUS.1$NAME=trimws(gsub(" County","",CENSUS.1$NAME))

names(CENSUS.1)[2] <- "County"

# Save map data
rm(fips, i, k, vars, acs)
save.image(file="./Output/CENSUS.1.RData")

library(tidycensus)
#Pre-defining variables to be used in loop

vars=c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003",
       "B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033",
       "B07001_049","B07001_065","B07001_081" )

states = c("minnesota", "north-dakota", "south-dakota", "nebraska", "montana")

fips=c(27,38,46,31,30)

# API Command
k = 1

for(i in fips){
  acs=get_acs(geography="county",
              variables = vars,
              state = i,
              year = 2019,
              geometry = TRUE)
  temp = acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area=st_area(temp)
  map = temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map"),map)
  
  k = k+1
  rm(temp, map)
}


CENSUS.2=rbind(minnesotacensus,`north-dakotacensus`,`south-dakotacensus`,
               nebraskacensus,montanacensus)
States=rbind(minnesotamap,`north-dakotamap`,`south-dakotamap`,nebraskamap,
             montanamap)

CENSUS.2$NAME=as.data.frame(str_split_fixed(CENSUS.2$NAME, ",", 2))[,1]
CENSUS.2$NAME=trimws(gsub(" County","",CENSUS.2$NAME))

save(States, file = "./Output/States.Rda") 

# Rename the NAME column
names(CENSUS.2)[2] <- "County"

# Save map data
rm(fips, i, k, states, vars, acs)
save.image(file="./Output/CENSUS.2.RData")

# Percentage change between 2016 and 2019
delta = function(x,y) {
  change = ((y-x)/x)*100
  return(change)
}

# Merging the two datasets
merged = cbind(CENSUS.1, CENSUS.2)

#Calculating percentage changes
merged = merged %>%
  mutate(pc_Male =  delta(merged$perMale, merged$perMale.1),
         pc_White = delta(merged$perWhite, merged$perWhite.1),
         pc_Black = delta(merged$perBlack, merged$perBlack.1),
         pc_Cit = delta(merged$perCit, merged$perCit.1),
         pc_Stay = delta(merged$perStay, merged$perStay.1),
         pc_SameCounty = delta(merged$perSameCounty, merged$perSameCounty.1),
         pc_SameSt = delta(merged$perSameSt, merged$perSameSt.1),
         pc_OthState= delta(merged$perOthState, merged$perOthState.1),
         pc_Abroad = delta(merged$perAbroad, merged$perAbroad.1))

# Creating CENSUS.3 from merged dataset and keeping the following variables
CENSUS.3 = merged[c("County", "state", "pc_Male", "pc_White", "pc_Black", "pc_Cit",
                    "pc_Stay", "pc_SameCounty", "pc_SameSt", "pc_OthState",
                    "pc_Abroad", "geometry")]

votes = with(votes, votes[order(votes$state, votes$County),])

CENSUS.3 = with(CENSUS.3, CENSUS.3[order(CENSUS.3$state, CENSUS.3$County),])

# Save CENSUS.3 data
save.image(file="./Output/CENSUS.3.RData")

core = cbind(CENSUS.3, votes)

# Drop repeated columns
core = subset(core, select = -c(County.1, state.1))
core$area<-st_area(core)
summary(core)

# (3)

map1=ggplot(core)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = States,
    fill = NA,
    colour = "black",
    size=1,
    inherit.aes = FALSE
  )

map2=ggplot(core)+ 
  geom_sf(aes(fill = pc_White))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = States,
    fill = NA,
    colour = "black",
    size=1,
    inherit.aes = FALSE
  )

plot_grid(map1,map2)

