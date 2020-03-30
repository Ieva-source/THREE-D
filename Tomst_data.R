##############################################################################
#####TEMPERATURE DATA#########################################################
##############################################################################
###TOMST LOGGERS###################

Temp<-read.csv("TomstLoggerMine.csv",              #the name of your file
             sep=";",                        #type of separator between columns in file
             dec=",",                        #decimal point or comma? 
             header=TRUE,                    #First row contains column names
             stringsAsFactors = FALSE)

View(Temp)

library(ggplot2)
theme_set(theme_minimal())

head(Temp)

pG <- ggplot(Temp, aes(x = Date, y = AirTemperature, fill = Site)) +
  geom_boxplot()
pG

pG <- ggplot(Temp, aes(x = Date, y = AirTemperature, fill = Site)) +
  geom_line()
pG

ggplot(data = Temp, aes(x = Date, y = AirTemperature))+
  geom_line()

###subseting data###
library(tidyverse)

str(Temp)