library(tidyverse)
covidIL= read.csv("Data/ILcovid19.csv")
DIF=function(x){
  boy=x-lag(x)
  return(boy)
}

change=function(x){
  girl=(x/lag(x)-1)*100
  return(girl)
}

covidIL=covidIL %>%
  mutate(New_test=DIF(Tests),
         New_cases=DIF(Cases),
         New_deaths=DIF(Deaths))
covidIL=covidIL %>%
  mutate(Pc_test=change(New_test),
         Pc_cases=change(New_cases),
         Pc_deaths=change(New_deaths))
covidIL$Date=as.Date(covidIL$Date,format="%m/%d/%Y")
plot(covidIL$Date, covidIL$Pc_cases,
     main="Fig.1 Daily Percentage Change in New Cases of Covid-19 in Illinois",
     xlab="Date",
     ylab="New Cases",
     type="l",
     col="blue"
)
plot(covidIL$Date, covidIL$Pc_test,
     main="Fig.2 Daily Percentage Change in Covid-19 Tests in Illinois",
     xlab= "Date",
     ylab= "Tests",
     type= "l",
     col= "green"
)
plot(covidIL$Date, covidIL$Pc_deaths,
     main="Fig.3 Daily Percentage Change in Covid-19 Deaths in Illinois",
     xlab= "Date",
     ylab= "Deaths",
     type= "l",
     col= "red"
)

#
