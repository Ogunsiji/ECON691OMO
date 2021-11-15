#This is a script for Assignment 3 Submitted to Prof Groves
#Created by Ogunsiji M.O. on October 1, 2021

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
library(stargazer)

# (1)

change = function(x, y) {
  diff = y-x
  return(diff)
}

# Importing data
#df = read.csv("./Data/ElectionData_Clean.csv")

df<-countypres_2000_2020 %>%
  filter(state %in% c("MINNESOTA", "NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA", "MONTANA"),
         party %in% c("REPUBLICAN", "DEMOCRAT"),
         year %in% c(2016, 2020)) %>%
  select(year, state, county_name, candidate, candidatevotes) %>%
  mutate(County=str_to_title(county_name)) %>%
  select(!county_name) %>%
  mutate(state=str_to_lower(state)) %>%
  pivot_wider(names_from = c(year, candidate), values_from = candidatevotes)

names(df)<-c("state","County","D_2016","R_2016","D_2020","R_2020")

D_VOTES<-df %>%
    mutate(c_Democratic=D_2020-D_2016,
           c_Republican=R_2020-R_2016,
           D_PCT_16 = D_2016/(D_2016+R_2016),
           R_PCT_16 = R_2016/(D_2016+R_2016),
           D_PCT_20 = D_2020/(D_2020+R_2020),
           R_PCT_20 = R_2020/(D_2020+R_2020))


load("./Output/votes.RData")
# Order data
#votes = votes[order(votes$state, votes$County),]
#df = df[order(df$state, df$County),]

# Change in Democratic votes
#df = df %>%
#  mutate(c_Democratic =  change(votes$Clinton, df$Democratic))

# Change in Republican votes
#D_VOTES = df %>%
#  mutate(c_Republican =  change(votes$Trump, df$Republican))


######################## PART 2 ########################

# Load and order CENSUS.2 data
load("./Output/CENSUS.2.RData") # from assignment two

CENSUS.2<-CENSUS.2 %>%
  mutate(state = gsub("-"," ",state))

CENSUS.2 = CENSUS.2[order(CENSUS.2$state, CENSUS.2$County),]

# Merging D_VOTES and CENSUS.2
#core = cbind(CENSUS.2, D_VOTES)

core<-merge(CENSUS.2, D_VOTES, by=c("state","County"))

load("./Output/States.Rda") 

# Graphs
map3 = ggplot(core) + 
  geom_sf(aes(fill = c_Republican))+
  scale_fill_gradient(low="white",high="red",
                      aes(name="Change in Republican votes"))+
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

map3

ggsave("./Plots/republican_votes_change.eps", plot = map3, device = "eps")

map4 = ggplot(core) + 
  geom_sf(aes(fill = c_Democratic))+
  scale_fill_gradient(low="white",high="blue",
                      aes(name="Change in Democratic votes"))+
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

map4

ggsave("./Plots/democratic_votes_change.eps", plot = map4, device = "eps")

# (3)

# CENSUS.3 data
load("./Output/CENSUS.3.RData") # from assignment two

CENSUS.3 = with(CENSUS.3, CENSUS.3[order(CENSUS.3$state, CENSUS.3$County),])

CENSUS.3<-CENSUS.3 %>%
  mutate(state = gsub("-"," ",state))

core$geometry<-NULL

# Merging core and CENSUS.3
#reg_data = cbind(CENSUS.3, core)
reg_data = merge(CENSUS.3, core, by=c("state","County"))

# Drop repeated columns
reg_data = subset(reg_data, select = -c(County.1,County.2, state.1, state.2, geometry.1))
summary(reg_data)

# Regression models
mod1=lm(c_Republican~perWhite+perMale, data=reg_data)
summary(mod1)

mod2=lm(c_Democratic~perWhite+perMale, data=reg_data)
summary(mod2)

mod3=lm(c_Republican~pc_White+pc_Male, data=reg_data)
summary(mod3)

mod4=lm(c_Democratic~pc_White+pc_Male, data=reg_data)
summary(mod4)

mod5=lm(c_Republican~pc_White+pc_Male+factor(state)-1, data=reg_data)
summary(mod5)

mod6=lm(c_Democratic~pc_White+pc_Male+factor(state)-1, data=reg_data)
summary(mod6)

