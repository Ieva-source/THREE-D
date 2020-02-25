#####################################################################
#####Plant community data (total abundances)#########################
#####################################################################

#******************************************#
#### Input data and install packages ####
#*****************************************#

library(vegan)   # For ordinations
library(vegan3d) # To plot ordinations in three dimensions
library(reshape2)# To rearrange and reshape data  
library(ggplot2)
library(gplots)

SP<-read.csv("plant_community_data_total.csv",              #the name of your file
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


#************************#
#### 1. ORDINATION####
#************************#

ord=metaMDS(sp,   #Carries out non-dimensional scaling of the data    
            k=5)  #How many dimensions you want - "Stress" should ideally be <0.05, or at least not higher than 0.2
##empty plot
ordiplot(ord,            ##Ordination object to 
         choices=c(1,2), ##Which axes
         type="n")       ##Plot text
##Plot species scores
text(ord,                      ##plot ordination
     choices = c(1,2),         ##which axes
     display="species",        ##Species scores
     cex=0.75)                 ##size of plot 
##Plot sites by Treatment type
points(ord,                    ##Plot points
       col=as.factor(env$Site),##Different colours per treatment type
       pch=25,                 ##Plot filled points
       choices = c(1,2),
       cex=0.75)       ##which dimensions from the NMDS analysis (we pick axes 1 and 2)

ordihull(ord,                   ##Plot the ordination
         as.factor(env$Site),##Joining the points attributable to particular Sites
         label = TRUE,          ##Label the shapes        
         choices=c(1,2))       ##which dimensions from the NMDS analysis (we pick axes 1 and 2)


legend("topleft",##Display the stress value in the topleft corner
       legend=round(ord$stress,5),
       box.lty = 0)##pick the number of dimensions (k) you chose)


legend("bottomright",                            ##Display a legend in the topright corner
       legend=c("Vikesland","Hogsete","Joasete","In Between","Liahovden"),##With this text
       pch=25,                                   ##And this character types
       col=c(5,1,3,2,4),
       title = "Study sites",
       box.lty = 0)                      ##in these colours


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
##B: - What has the greatest number of different taxa between them - trap or habitat?

betadiver(sp.Treatment,"w")       #calculates beta diversity of a community data frame. 
betadiver(sp.Site ,"w")       #"w" is whitakers beta diversity.  (Total species/mean species per plot)-1. 

#*****************************************#
########## 2. Richness Indices ############
#*****************************************#

#### QUESTION C - Was enough sampling effort made in each site and across all study sites 
#to capture the whole plant community?


###You will need to make plots with multiple curves

##species accumualtion curves

#example for trap-types. Change to compare habitats.

sp.acc.site.Vikesland=specaccum(sp,         ##Community data frame  - give appropriate name reflecting "trap" or "habitat"
                                subset =  env$Site=="Vikesland", ##The factor in the env data frame "habitat" or "trap", and the level of that factor you want to subset by.
                                method ="random")        ##The method to calculate the the curve - accept it.

sp.acc.site.Hogsete=specaccum(sp,         ##Community data frame  - give appropriate name reflecting "trap" or "habitat"
                              subset =  env$Site=="Hogsete", ##The factor in the env data frame "habitat" or "trap", and the level of that factor you want to subset by.
                              method ="random")        ##The method to calculate the the curve - accept it.

sp.acc.site.Joesete=specaccum(sp,         ##Community data frame   - give appropriate name reflecting "trap" or "habitat"
                              subset =  env$Site=="Joesete", ##The factor in the env data frame "habitat" or "trap", and the level of that factor you want to subset by.
                              method ="random")         ##The method to calculate the the curve - accept it.

sp.acc.site.In.Between=specaccum(sp,         ##Community data frame   - give appropriate name reflecting "trap" or "habitat"
                                 subset =  env$Site=="In Between", ##The factor in the env data frame "habitat" or "trap", and the level of that factor you want to subset by.
                                 method ="random")         ##The method to calculate the the curve - accept it.

sp.acc.site.Liahovden=specaccum(sp,         ##Community data frame   - give appropriate name reflecting "trap" or "habitat"
                                subset =  env$Site=="Liahovden", ##The factor in the env data frame "habitat" or "trap", and the level of that factor you want to subset by.
                                method ="random")         ##The method to calculate the the curve - accept it.

plot(sp.acc.site.Vikesland, ##species accumulation curve object for a particular trap/habitat-type
     col=1,        ##colour of line
     add=FALSE)    

##The additional data-series have to be plotted afterwards by adding lines to the open plot:     

lines(sp.acc.site.Hogsete,        ##species accumulation curve object for a particular trap/habitat-type
      col=2)    

lines(sp.acc.site.Joesete,        ##species accumulation curve object for a particular trap/habitat-type
      col=3)    

lines(sp.acc.site.In.Between,        ##species accumulation curve object for a particular trap/habitat-type
      col=4) 

lines(sp.acc.site.Liahovden,        ##species accumulation curve object for a particular trap/habitat-type
      col=5) 

legend("bottomright",    ##position of legend
       legend=c("Vikesland",  ##Name of trap/habitat - change appropriately
                "Hogsete", "Joasete", "In Between", "Liahovden"),
       fill=c(1, 2, 3, 4, 5))   ##Colour of lines

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

diversity(sp.Site,"simpson")   ###Calculates a particular diversity index 

#QUESTION D: Are you more likely to catch particular taxa in different habitats/traps?
#QUESTION E: Is the species community dominated by a few abundant species, and does 
#this effect differ between trap and/or habitat types?
#QUESTION F: Which trap and habitat type has the highest Simpson's diversity index?

#******************************************************************************#
#### 4. Dissimilarity,clustering, presence/absence and comparing ordinations####
#******************************************************************************#

##Like the vegetation data we can cluster our data.  The code is the same as 
##the vegetation practical.

#DISSIMILARITY MATRIX 
sp.dist<-vegdist(sp,  #excluding the highly abundant taxa
                 method="bray")         #default method is Bray-Curtis = "bray". 
#"raup" is based on presence and absence and good to use if you are comparing data including highly abundant and rare taxa


#CLUSTERING
tr<-hclust(sp.dist,                 #dissimilarity matrix based on raw abundance)
           method="average")    #type of clustering


plot(tr,                  #Plot your tree
     hang = -1)            #site names all at the same level

##Can't make this work##

tabasco(sp,  #community data
        tr,  #clustering tree
        Colv = FALSE)


#QUESTION D: Are you more likely to catch particular taxa in different habitats/traps?
#QUESTION E: Is the species community dominated by a few abundant species, and does 
#this effect differ between trap and/or habitat types?
#QUESTION F: Which trap and habitat type has the highest Simpson's diversity index?


#*****************************************************************************************************************
## 5. Compare Ordination using binary data composition with what you did using absolute abundance data (see: "ORDINATION" at the start). 
#*****************************************************************************************************************
####Question H - How does the presence/absence data differ from the abundance data? Plot as you did for absolute abundance data

##Creating a presence absence data frame
sp.binary=sp               
sp.binary[sp.binary>0]=1  ##everything that is not zero is re-written to a presence

ord.bin=metaMDS(sp.binary,k=5) #ordinates the data in the same number of dimensions

##empty plot
ordiplot(ord.bin,            ##Ordination object to 
         choices=c(1,2), ##Which axes
         type="n")       ##Plot text
##Plot species scores
text(ord.bin,                      ##plot ordination
     choices = c(1,2),         ##which axes
     display="species",        ##Species scores
     cex=0.75)                 ##size of plot (THIS IS NOT FONT SIZE)
##Plot sites
points(ord.bin,                    ##Plot points
       col=as.factor(env$Site),##Different colours per trap type
       pch=20,                 ##Plot filled points
       choices = c(1,2))       ##which axes

ordihull(ord.bin,                   ##PLot the ordination
         as.factor(env$Site),##Joining the points attributable to particular habitats
         label = TRUE,          ##Label the shapes        
         choices=c(1,2))        ##PLot the first two axes

legend("topleft",##Display the stress value in the topleft corner
       legend=round(ord.bin$stress,5))##Rounded to 4 decimal plces


legend("topright",                            ##Display a legend in the topright corner
       legend=c("Vikesland","Hogsete","Joasete", "In Between", "Liahovden"),##With this text
       pch=20,                                ##And this character types
       col=c(1,2,3,4,5))                            ##in these colours


####Question H - How does the presence/absence data differ from the abundance data?
##Direct comparison of the ordinations
##It is posible to fit one ordination over the other using procrustes analysis.
##It can tell you how closely related two ordinations are.
##The additional test offers a way of testing  how likley that the relationship 
##occurs by randonm chance.  The results give you Sums of Squares, a correlation coefficient and a P value

####Question I: What do the correlation coefficient and p-value mean?####

pro=procrustes(ord,ord.bin)    #Rotates the ordinations to maximise similarity between two ordinations
plot(pro,type="t")             #plots the two ordinations relative to each other after rotation. Arrows point by default to the target matrix (in this case "ord"=the original ordination based on count abundance data)
protest(ord,ord.bin)           #Gives the probability that the observed similarity between the two ordinations is generated purely by chance
#through random permutation

#*************************************#
#### 6. Hypothesis testing ####
#*************************************#


####Question J: Are there differences observed between sites?
##Adonis carries out a MANOVA (an anova with Multiple response/dependent variable) on distances
## against environmental variables. It then uses permutations to see how likely the test statistic
## is to arrive by random chance. This is a form of permutation test.


test=adonis(data=env,    #Environmental dataframe
            sp ~     #Distance matrix
              Site*Treatment)#Testing variables
test
