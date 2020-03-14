#******************************************#
#### Input data and install packages ####
#*****************************************#

library(vegan)   # For ordinations
library(vegan3d) # To plot ordinations in three dimensions
library(reshape2)# To rearrange and reshape data  
library(ggplot2)
library(gplots)

SP<-read.csv("Score_damage.csv",              #the name of your file
             sep=";",                        #type of separator between columns in file
             dec=",",                        #decimal point or comma? 
             header=TRUE,                    #First row contains column names
             stringsAsFactors = FALSE)       #All test occurs as strings


str(SP)

env=SP[,1:3]                                 #Create a data frame of information about plots

str(env)

mel.sp<-melt(SP                              #converts the data from the community data table type
             ,id=c("PlotID","Site","Treatment"),   #to a table format. id= Identifies columns not to 'melt'
             value.name = "abund")            #value.name gives a name to the created column

names(mel.sp)=c("PlotID","Site","Treatment","species","abund") #renames the columns


str(mel.sp)               ##examine the structure of mel.sp

sp=SP[,4:length(SP)]      ##Creates sp removeing none species related data
env=env[rowSums(sp)!=0,]  ##Removes rows with no species data from env
sp=sp[rowSums(sp)!=0,]    ##Removes rows with no species data from sp
rownames(sp)=env$id       ##Gives the rows names
rownames(env)=env$id

###You will also need to create two more data frames. One for traps and one for habitat 
###This is done using the mel.sp data frame. Xtabs creates a matrix which need to be 
###converted to a data frame


sp.Site=as.data.frame.matrix(xtabs(data=mel.sp,#this will produce an appropriate community table using the melted data
                                   abund~       #these are the values to fill the data frame  
                                     Site+      #ROWS
                                     species))   #Columns 

sp.Treatment=as.data.frame.matrix(xtabs(data=mel.sp,#this will produce an appropriate community table using the melted data
                                        abund~       #these are the values to fill the data frame  
                                          Treatment+      #ROWS
                                          species))   #Columns 
#*****************************************#
####### 3. Rank abundance curves ##########
#*****************************************#

##Are we more likely to find particular species in different sites?


#Code below is for the effect of sites. 

sp.sum.site=aggregate(data=mel.sp,       ##creates a data frame summarising the data in book format by averages.
                      abund~Site*species,   ##a formula of the summary we are interested in i.e abund the right side of the formula is rows*columns
                      FUN=function(x){    ##a function or list of functons we are interested in summarising
                        sum(x)   
                      })##the function to apply to x

###THERE IS A HEALTHY BIT OF R CODE HERE, WHAT ITS DOING IS EXPLAINED IN THE COMMENTS (but remember to focus on the ecology). 

##here we split the data frame into a list of data frames by what we are interested in i.e. habitat or trap
sp.sum.site=split(sp.sum.site,      
                  sp.sum.site$Site)             #The column we wish to split our data frame by

sp.sum.site=lapply(sp.sum.site,function(x){    #lapply fits functions to dataframes
  data.frame(x,                              #our function will create a data frame with the original information
             rank=rank(-x$abund,ties="first") #the abundance rank of the data. where values are equal they will be ranked
  )})


sp.sum.site=do.call(rbind,sp.sum.site)  #EXTREMELY USEFUL CODE calls rbind to a list turning the list back to a data frame



ggplot(data=sp.sum.site,          ###start ggplot
       aes(x=rank,               ###x values are rank
           y=(abund),          ###y values are abundance - IT IS POSSIBLE TO LOG TRANSFORM THIS HERE
           colour=Site))+     ###colour by this factor
  geom_point()+                  ###produce points
  geom_line()+                   ###draw a line  
  scale_y_continuous(trans = "identity")+##scale on y axis have a look at ?scale_y_continuous
  ggtitle("Rank-Abundance Curve")+  ##Chart title
  xlab("Rank")+                     ##x axis label
  ylab("Abundance")+                ##y axis label
  theme(axis.text.x = element_text(size=12),###Axis font size
        axis.text.y = element_text(size=12),###Axis font size
        plot.title = element_text(size=24),###title font size
        axis.title.x=element_text(size=16),###Axis title font size
        axis.title.y=element_text(size=16,angle=90),###Axis title font size and rotation
        legend.position = "bottom")###PLacement of legend

sp.sum.site=sp.sum.site[order(sp.sum.site$rank),]  ##Orders the data frame by rank

subset(sp.sum.site,subset = sp.sum.site$Site=="Vikesland") ##Displays the data from a particular subset
subset(sp.sum.site,subset = sp.sum.site$Site=="Hogsete") ##Displays the data from a particular subset
subset(sp.sum.site,subset = sp.sum.site$Site=="Joesete")
subset(sp.sum.site,subset = sp.sum.site$Site=="In Between")
subset(sp.sum.site,subset = sp.sum.site$Site=="Liahovden")##Displays the data from a particular subset
