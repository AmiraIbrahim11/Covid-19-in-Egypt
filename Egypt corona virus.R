#Import Data 
library(readr)
Egypt_coronavirus <- read_csv(file.choose())
View(Egypt_coronavirus)

#Loading libraries 
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(gridExtra)

#Explore Data 
glimpse(Egypt_coronavirus)

Egypt_Coronavirus<- Egypt_coronavirus %>%
  mutate(Date=dmy(Date))

##Covid-19 in Egypt from 14 Feb 2020 till 12 May 2020 :
a <- ggplot(Egypt_Coronavirus,aes(x=Date,y=Total_cases,color="black"))+
      geom_line()+
      labs(title = "Covid-19 in Egypt from 14-Feb till 12 May",y="Number Of Cases")+
      geom_line(aes(x=Date,y=Total_deaths,color="red"))+
      geom_line(aes(x=Date,y=Total_recovered,color="blue"))+
      geom_line(aes(x=Date,y=Active_cases,color="green"))+
      scale_color_discrete(name = "", labels = c("Total Cases", "Total Recovered","Active Cases",
                                                "Total Deaths"))+
      theme(legend.position = "top")

##Newly cases,newly Deaths,newly Recovered
b <- ggplot(Egypt_Coronavirus,aes(x=Date,y=New_cases,color="black"))+
      geom_line()+
      geom_line(aes(x=Date,y=New_deaths,color="red"))+
      geom_line(aes(x=Date,y=New_recovered,color="blue"))+
      scale_color_discrete(name = "", labels = c("New Cases", "New Recovered",
                                             "New Deaths"))+
      theme(legend.position = "top")

##Bar plot (Deaths Acc to total cases)
Bar_plot_Data <- gather(Egypt_Coronavirus,event,total,c(Total_cases,Total_deaths))
c <- Bar_plot_Data%>%
        ggplot()+
         geom_bar(aes(y=total,x=Date,fill=event),stat = "identity",position = "dodge")+
         labs(y="Number Of Cases")+
         theme(legend.position = "top")

##Bar plot2
Bar_plot2_Data <- gather(Egypt_Coronavirus,event,total,c(Total_cases,Total_deaths,Total_recovered))

d <- ggplot(Bar_plot2_Data , aes(y=total,x=Date,fill=event))+
       geom_col(position = "fill")+
       theme(legend.position = "top")
  

##Bar plot3 
Bar_plot3_Data <- gather(Egypt_Coronavirus,event,total,c(Total_deaths,Total_recovered))

e <- Bar_plot3_Data%>%
       ggplot()+
       geom_bar(aes(y=total,x=Date,fill=event),stat = "identity",position = "dodge")+
       theme(legend.position = "top")


#creat a grid :
grid.arrange(a, arrangeGrob(b, d, ncol = 2), nrow = 2)

grid.arrange(a,b, arrangeGrob(c, d,e, ncol = 3), nrow = 3)