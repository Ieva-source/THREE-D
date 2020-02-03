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
             
names(mel.sp)=c("Site","PlotID","Treatment","Species","Abund") #renames the columns


str(mel.sp)               ##examine the structure of mel.sp

#Still working on this:

sp=SP[,4:length(SP)]      ##Creates sp removeing none species related data
env=env[rowSums(sp)!=0,]  ##Removes rows with no species data from env
sp=sp[rowSums(sp)!=0,]    ##Removes rows with no species data from sp
rownames(sp)=env$id       ##Gives the rows names
rownames(env)=env$id

######################################################
#Ploting the biomass data##############################

#Boxplots for the total biomass of the permanent plots:

Permanent<-read.csv("Total_Biomass_Permanent_Plots_Final.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)        
             
boxplot(Total.Biomass~Site, data=Permanent, main="Total above-ground biomass in permanent plots", xlab="Study sites and treatment", ylab="Total above-ground biomass (g)")

#Tried the Notched Boxplot as well:

boxplot(Total.Biomass~Site*Treatment, data=Permanent, notch=TRUE, col=(c("gold","darkgreen")), main="Total biomass in permanent plots", xlab="Study sites and treatment", ylab="Total biomass")

#Boxplots for the total biomass of the temporary plots in Vikesland:

Vikesland.Temporary<-read.csv("Biomas_Temporary_Vikesland.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)  

boxplot(Total~Treatment.Time, data=Vikesland.Temporary, main="Total above-ground biomass in temporary plots in Vikesland", xlab="Treatment x Time", ylab="Total above-ground biomass (g)")

#Boxplots for the total biomass of the temporary plots in Hogsete:

Hogsete.Temporary<-read.csv("Biomass_Temporary_Hogsete.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)  

boxplot(Total~Treatment.Time, data=Hogsete.Temporary, main="Total above-ground biomass in temporary plots in Hogsete", xlab="Treatment x Time", ylab="Total above-ground biomass (g)")

#Boxplots for the total biomass of the temporary plots in Joesete:

Joesete.Temporary<-read.csv("Biomass_Temporary_Joesete.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)  

boxplot(Total~Treatment.Time, data=Joesete.Temporary, main="Total above-ground biomass in temporary plots in Joesete", xlab="Treatment x Time", ylab="Total above-ground biomass (g)")

#Boxplots for the total biomass of the temporary plots in Between:

Between.Temporary<-read.csv("Biomass_Temporary_Between.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)  

boxplot(Total~Treatment.Time, data=Between.Temporary, main="Total above-ground biomass in temporary plots in Between", xlab="Treatment x Time", ylab="Total above-ground biomass (g)")

#Boxplots for the total biomass of the temporary plots in Liahovden:

Liahovden.Temporary<-read.csv("Biomass_Temporary_Laihovden.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)  

boxplot(Total~Treatment.Time, data=Liahovden.Temporary, main="Total above-ground biomass in temporary plots in Liahovden", xlab="Treatment x Time", ylab="Total above-ground biomass (g)")

#############################################################
##############Boxolots for functional groups#################

#Boxplots for the gramminoids in the permanent plots:

Functional.Permanent<-read.csv("Functional_Group_Biomass_Permanent_Plots.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)        
             
boxplot(Graminoids~Site.Treatment, data=Functional.Permanent, main="Graminoid above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")

#Boxplots for the forbs in the permanent plots:

             
boxplot(Forbs~Site.Treatment, data=Functional.Permanent, main="Forb above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")

#Boxplots for the bryophytes in the permanent plots:

             
boxplot(Bryophytes~Site.Treatment, data=Functional.Permanent, main="Bryophyte above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")

#Boxplots for the lichen in the permanent plots:

             
boxplot(Lichen~Site.Treatment, data=Functional.Permanent, main="Lichen above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")

#Boxplots for the shrubs in the permanent plots:

             
boxplot(Shrubs~Site.Treatment, data=Functional.Permanent, main="Shrub above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")

#Boxplots for the litter in the permanent plots:

             
boxplot(Litter~Site.Treatment, data=Functional.Permanent, main="Litter above-ground biomass in permanent plots", xlab="Study sites and treatments", ylab="Total above-ground biomass (g)")
