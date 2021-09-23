#This is a script for Assignment 2 Submitted to Prof Groves
#Created by Ogunsiji M.O. on September 21, 2021

rm(list=ls())
library(rvest)
library(tidyverse)

#List of the states for which data will be pulled

states<-c("south-dakota", "north-dakota","montana","minnesota","nebraska")

for(i in states){
  #Specifying the URL for desired website to be scraped
  url.1 <- "https://www.nytimes.com/elections/2016/results/"
  url<-paste0(url.1,i)
  webpage <- read_html(url)
  tables<-webpage %>%
    html_nodes("table") #This pulls out all the "table" nodes in the HTML code
  results<-tables[2] %>%
    html_table(fill=TRUE,header=TRUE) %>%
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton" = (Clinton)/(Clinton+Trump),
           "pctTrump" = Trump/(Clinton+Trump))
  assign(i,results)
}

## Adding state name to each of the states' dataframe

df_SD <- data.frame(append(`south-dakota`, c(state='SOUTH-DAKOTA'), after=1))
df_ND <- data.frame(append(`north-dakota`, c(state='NORTH-DAKOTA'), after=1))
df_MO <- data.frame(append(montana, c(state='MONTANA'), after=1))
df_MI <- data.frame(append(minnesota, c(state='MINNESOTA'), after=1))
df_NE <- data.frame(append(nebraska , c(state='NEBRASKA'), after=1))

## Creating a single dataframe called VOTES with required information

df_list <- list(df_SD,df_ND,df_MO,df_MI,df_NE)

VOTES <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)

# Alternatively: this will produce a single dataframe state by state
VOTES1 <- do.call("rbind", list(df_SD,df_ND,df_MO,df_MI,df_NE))


### PART II

require(tidyverse)
require(dplyr)
require(rvest)
require(cowplot)
require(ggplot2)
library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002",
        "B02001_003","B05001_001","B05001_006","B07001_001",
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

#Command to pull data from ACS for 2016 


census_api_key("1d1ef57c663079899a989939cd480995e2eb1f4e",overwrite=T,install = TRUE)
readRenviron("~/.Renviron")

#Data file and calculation for South-Dakota

CENSUS_SD <- get_acs(geography = "county", variables = vars, state = 46, year = 2016, geometry = TRUE)

SD2016.acs<-CENSUS_SD %>%
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
  spread(key=variable2, value=estimate)

SD2016_PCT<- SD2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_SD19 <- get_acs(geography = "county", 
                     variables = vars, 
                     state = 46,  
                     year = 2019, 
                     geometry = TRUE)
SD2019.acs<-CENSUS_SD19 %>%
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
  spread(key=variable2, value=estimate)

SD2019_PCT<- SD2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")

#Data file and calculation for North-Dakota

CENSUS_ND <- get_acs(geography = "county", 
                     variables = vars, 
                     state = 38,  
                     year = 2016, 
                     geometry = TRUE) 
ND2016.acs<-CENSUS_ND %>%
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
  spread(key=variable2, value=estimate)

ND2016_PCT<- ND2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_ND19 <- get_acs(geography = "county", 
                       variables = vars, 
                       state = 38,  
                       year = 2019, 
                       geometry = TRUE) 
ND2019.acs<-CENSUS_ND19 %>%
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
  spread(key=variable2, value=estimate)

ND2019_PCT<- ND2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for Montana

CENSUS_MO <- get_acs(geography = "county", 
                     variables = vars, 
                     state = 30,  
                     year = 2016, 
                     geometry = TRUE) 

MO2016.acs<-CENSUS_MO %>%
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
  spread(key=variable2, value=estimate)

MO2016_PCT<- MO2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_MO19 <- get_acs(geography = "county", 
                       variables = vars, 
                       state = 30,   
                       year = 2019, 
                       geometry = TRUE) 

MO2019.acs<-CENSUS_MO19 %>%
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
  spread(key=variable2, value=estimate)

MO2019_PCT<- MO2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for Minnesota

CENSUS_MI <- get_acs(geography = "county", variables = vars, 
                     state = 27,  
                     year = 2016, 
                     geometry = TRUE) 
MI2016.acs<-CENSUS_MI %>%
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
  spread(key=variable2, value=estimate)

MI2016_PCT<- MI2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_MI19 <- get_acs(geography = "county", 
                       variables = vars, 
                       state = 27,  
                       year = 2019, 
                       geometry = TRUE)
MI2019.acs<-CENSUS_MI19 %>%
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
  spread(key=variable2, value=estimate)

MI2019_PCT<- MI2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for Nebraska

CENSUS_NE <- get_acs(geography = "county", 
                     variables = vars, 
                     state = 31,  
                     year = 2016, 
                     geometry = TRUE)

NE2016.acs<-CENSUS_NE %>%
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
  spread(key=variable2, value=estimate)

NE2016_PCT<- NE2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_NE19 <- get_acs(geography = "county", 
                       variables = vars, 
                       state = 31,  
                       year = 2019, 
                       geometry = TRUE)
NE2019.acs<-CENSUS_NE19 %>%
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
  spread(key=variable2, value=estimate)

NE2019_PCT<- NE2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


SD2016_PCT$name <- SD2016.acs$NAME
SD2019_PCT$name <- SD2019.acs$NAME
ND2016_PCT$name <- ND2016.acs$NAME
ND2019_PCT$name <- ND2019.acs$NAME
MO2016_PCT$name <- MO2016.acs$NAME
MO2019_PCT$name <- MO2019.acs$NAME
MI2016_PCT$name <- MI2016.acs$NAME
MI2019_PCT$name <- MI2019.acs$NAME
NE2016_PCT$name <- NE2016.acs$NAME
NE2019_PCT$name <- NE2019.acs$NAME


CENSUS.1 <- do.call("rbind", list(SD2016_PCT, ND2016_PCT, MO2016_PCT, MI2016_PCT, NE2016_PCT))
save(list = "CENSUS.1", file = "C:/Users/user/Documents/ECON691OMO/CENSUS.1.RData")

CENSUS.2 <- do.call("rbind", list(SD2019_PCT, ND2019_PCT, MO2019_PCT, MI2019_PCT, NE2019_PCT))
save(list = "CENSUS.2", file = "C:/Users/user/Documents/ECON691OMO/CENSUS.2.RData")

## PART III

library(ggplot2)
require(cowplot)
p1_SD<-ggplot(SD2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (SD)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_SD<-ggplot(SD2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (SD)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_SD,p2_SD)

p1_ND<-ggplot(ND2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (ND)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_ND<-ggplot(ND2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (ND)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_ND,p2_ND)

p1_MO<-ggplot(MO2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (MO)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_MO<-ggplot(MO2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (MO)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_MO,p2_MO)

p1_MI<-ggplot(MI2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (MI)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_MI<-ggplot(MI2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (MI)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_MI,p2_MI)

p1_NE<-ggplot(NE2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (NE)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_NE<-ggplot(NE2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (NE)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_NE,p2_NE)


plot_grid(p1_SD,p2_SD,p1_ND,p2_ND,p1_MO,p2_MO,p1_MI,p2_MI,p1_NE,p2_NE, nrow = 3)

