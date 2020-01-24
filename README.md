# THREE-D

#Import and install packages needed to check whether enough sampling effort has been made
#To make species accumulation curves, rank abundance, calculate diversity indices, #ordinate and clure the data
library (vegan)  # For ordinations
library(vegan3d) # To plot ordinations in three dimensions- BEWARE Apple users.
library(reshape2)# To rearrange and reshape data  
library(ggplot2)
library(gplots)

SP<-read.csv("plant_community_data_total.csv",  #the name of the data file
             sep=";",                        #type of separator between columns in file
             dec=",",                        #decimal comma 
             header=TRUE,                    #First row contains column names
             stringsAsFactors = FALSE)        #All test occurs as strings
             
str(SP)

env=SP[,1:3]                            #Create a data frame of information about plots

str(env) 

mel.sp<-melt(SP                    #converts the data from the community data table type
             ,id=c("Site","PlotID","Treatment"), #to a table format. id= Identifies columns not to 'melt'
             value.name = "abund")      #value.name gives a name to the created column
             
