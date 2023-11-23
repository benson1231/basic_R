---
title: "Visualizing Carbon Dioxide Levels"
output: html_notebook
---

```{r message=FALSE, warning=FALSE, error=TRUE}
# load libraries and data
library(readr)
library(dplyr)
library(ggplot2)
```

```{r error=TRUE}
options(scipen=10000) #removes scientific notation
#Create NOAA Visualization here:
noaa_data<-read.csv("carbon_dioxide_levels.csv")
head(noaa_data)
noaa_viz<-ggplot(data=noaa_data,aes(x=Age_yrBP,y=CO2_ppmv))+
            geom_line()+labs(title="Carbon Dioxide Levels From 8,000 to 136 Years BP",subtitle="From World Data Center for Paleoclimatology and NOAA Paleoclimatology Program",x="Years Before Today (0=1950)",y="Carbon Dioxide Level (Parts Per Million)")+scale_x_reverse(lim=c(800000,0))
noaa_viz
```

```{r message=FALSE, error=TRUE}
#Create IAC Visualization
iac_data<-read.csv("yearly_co2.csv")
head(iac_data)
millennia_max<- max(noaa_data$CO2_ppmv)
millennia_max
#the data_mean_global is an equivalent metric to CO2_ppmv
iac_viz<-ggplot(data=iac_data,aes(x=year,y=data_mean_global))+
geom_line()+labs(title="Carbon Dioxide Levels over Time",subtitle="From Institute for Atmospheric and Climate Science (IAC).",x="Year",y="Carbon Dioxide Level (Parts Per Million)")
iac_viz<-iac_viz+geom_hline(aes(yintercept=millennia_max,linetype ="Historical CO2 Peak before 1950"))
iac_viz


```
