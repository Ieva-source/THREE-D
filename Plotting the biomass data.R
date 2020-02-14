######################################################
#Ploting the biomass data##############################

#Boxplots for the total biomass of the permanent plots:

Permanent<-read.csv("Total_Biomass_Permanent_Plots.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)        

Permanent$elevation<-factor(Permanent$elevation,
                    labels = c("VO","HO","JC","BC","LC"))

boxplot(bioperday~elevation, data=Permanent, main="Biomass per day in permanent plots", xlab="Elevation and treatment", ylab="Biomass/day (g)", col=rainbow(2))

##Just saving the code##
Permanent$elevation<-factor(Permanent$elevation,
                            labels = c("469O","469C","700O","700C","920O","920C","1100O","1100C","1290O","1290C"))

#Now with standardized data (biomass/day)####
#The below line is failed:##
elevation<- factor(elevation, level=c("VO","VC","HO","HC","JO","JC","BO","BC","LO","LC"))

boxplot(bioperday~elevation, data=Permanent, main="Biomass/day in permanent plots", xlab="Study sites and treatment", ylab="Biomass/day (g)", col=rainbow(2))

###Trying the ggplot instead with standardized data set###

P<-read.csv("Testing.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)  

library(ggplot2)

P$elevation<-factor(P$elevation,
                    labels = c("469","700","920","1100","1290"))

p10<-ggplot(P, aes(x = elevation, y = bioperday)) +
  geom_boxplot()
p10
#Tried the Notched Boxplot as well:

boxplot(Total.Biomass~Site*Treatment, data=Permanent, notch=TRUE, col=(c("gold","darkgreen")), main="Total biomass in permanent plots", xlab="Study sites and treatment", ylab="Total biomass")

#Boxplots for the total biomass of the temporary plots in Vikesland:

Vikesland.Temporary<-read.csv("Biomas_Temporary_Vikesland.csv",  
                              sep=";",                        
                              dec=",",                        
                              header=TRUE,                    
                              stringsAsFactors = FALSE)  
par(mfrow=c(1,2))

boxplot(Totalbiomass~Harvest.nr, data=Vikesland.Temporary, main="Above-ground biomass in Vikesland", xlab="Harvest Nr x treatment", ylab="Above-ground biomass (g)", col=c(4,2))

legend(x=0.1, y=29.5,
       legend=c("Cage","Open"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(bioperday~Harvest.nr, data=Vikesland.Temporary, main="Biomass/day in Vikesland", xlab="Harvest Nr x treatment", ylab="Biomass/day (g)", col=c(4,2))

legend(x=-5,y=29.5,
       legend=c("Cage","Open"),
       col=c(4,2),
       pch=15,
       bty="n")

#Boxplots for the total biomass of the temporary plots in Hogsete:

Hogsete.Temporary<-read.csv("Biomass_Temporary_Hogsete.csv",  
                            sep=";",                        
                            dec=",",                        
                            header=TRUE,                    
                            stringsAsFactors = FALSE)  

par(mfrow=c(1,2))

boxplot(Total~Harvest.nr, data=Hogsete.Temporary, main="Above-ground biomass in tempo.plots in Hogsete", xlab="Harvest nr. x Treatment", ylab="Total above-ground biomass (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(bioperday~Harvest.nr, data=Hogsete.Temporary, main="Biomass/day in temporary plots in Hogsete", xlab="Harvest nr. x Treatment", ylab="Biomass/day (g)", col=c(2,4))

legend(x="topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

#Boxplots for the total biomass of the temporary plots in Joesete:

Joesete.Temporary<-read.csv("Biomass_Temporary_Joesete.csv",  
                            sep=";",                        
                            dec=",",                        
                            header=TRUE,                    
                            stringsAsFactors = FALSE)  

par(mfrow=c(1,2))

boxplot(Total~Harvest.nr, data=Joesete.Temporary, main="Above-ground biomass in tempo.plots in Joasete", xlab="Harvest nr. x Treatment", ylab="Total above-ground biomass (g)", col=c(2,4))

legend("topright",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(bioperday~Harvest.nr, data=Joesete.Temporary, main="Biomass/day in temporary plots in Joasete", xlab="Harvest nr. x Treatment", ylab="Biomass/day (g)", col=c(2,4))

legend(x="topright",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

#Boxplots for the total biomass of the temporary plots in Between:

Between.Temporary<-read.csv("Biomass_Temporary_Between.csv",  
                            sep=";",                        
                            dec=",",                        
                            header=TRUE,                    
                            stringsAsFactors = FALSE)  

par(mfrow=c(1,2))

boxplot(Total~Harvest.nr, data=Between.Temporary, main="Above-ground biomass in tempo.plots in Between", xlab="Harvest nr. x Treatment", ylab="Total above-ground biomass (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(bioperday~Harvest.nr, data=Between.Temporary, main="Biomass/day in temporary plots in Between", xlab="Harvest nr. x Treatment", ylab="Biomass/day (g)", col=c(2,4))

legend(x="topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

#Boxplots for the total biomass of the temporary plots in Liahovden:

Liahovden.Temporary<-read.csv("Biomass_Temporary_Laihovden.csv",  
                              sep=";",                        
                              dec=",",                        
                              header=TRUE,                    
                              stringsAsFactors = FALSE)  

par(mfrow=c(1,2))

boxplot(Total~Harvest.nr, data=Liahovden.Temporary, main="Above-ground biomass in tempo.plots in Liahovden", xlab="Harvest nr. x Treatment", ylab="Total above-ground biomass (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(bioperday~Harvest.nr, data=Liahovden.Temporary, main="Biomass/day in temporary plots in Liahovden", xlab="Harvest nr. x Treatment", ylab="Biomass/day (g)", col=c(2,4))

legend(x="topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

#############################################################
##############Boxolots for functional groups#################
#########################in permanent plots##################

#Boxplots for the gramminoids in the permanent plots:

Functional.Permanent<-read.csv("Functional_Group_Biomass_Permanent_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)        


par(mfrow=c(2,2))

boxplot(Gperday~Site.Treatment, data=Functional.Permanent, main="Graminoid Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("bottomright",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(Fperday~Site.Treatment, data=Functional.Permanent, main="Forb Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(Bperday~Site.Treatment, data=Functional.Permanent, main="Bryophyte Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(Lperday~Site.Treatment, data=Functional.Permanent, main="Lichen Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

#############################################################
##############Boxplots for functional groups#################
#########################in temporary plots##################

#############Vikesland#######################################

#Boxplots for the graminoids in the tempo plots in Vikesland:

V<-read.csv("Biomas_Temporary_Vikesland.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

boxplot(Gperday~Harvest.nr, data=V, main="Graminoid Biomass/day in permanent plots",xlab="Harvest nr. and treatments",ylab="Biomass/day (g)", col=c(2,4))


boxplot(Fperday~Site.Treatment, data=Functional.Permanent, main="Forb Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(Bperday~Site.Treatment, data=Functional.Permanent, main="Bryophyte Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")

boxplot(Lperday~Site.Treatment, data=Functional.Permanent, main="Lichen Biomass/day in permanent plots", xlab="Study sites and treatments", ylab="Biomass/day (g)", col=c(2,4))

legend("topleft",
       legend=c("Open","Cage"),
       col=c(4,2),
       pch=15,
       bty="n")


#Boxplots for the forbs in the tempo plots in Vikesland:

Vik.forb<-Functional.Temporary[c(1:18),c(4,6)]

boxplot(Forbs~Site.Treatment.Time, data=Vik.forb, main="Forb above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Bryophytes in the tempo plots in Vikesland:

Vik.bryo<-Functional.Temporary[c(1:18),c(4,7)]

boxplot(Bryophytes~Site.Treatment.Time, data=Vik.bryo, main="Bryophyte above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Lichen in the tempo plots in Vikesland:

Vik.lich<-Functional.Temporary[c(1:18),c(4,8)]

boxplot(Lichen~Site.Treatment.Time, data=Vik.lich, main="Lichen above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for Shrubs in the tempo plots in Vikesland:

Vik.shrub<-Functional.Temporary[c(1:18),c(4,9)]

boxplot(Shrubs~Site.Treatment.Time, data=Vik.shrub, main="Shrub above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Litter in the tempo plots in Vikesland:

Vik.litter<-Functional.Temporary[c(1:18),c(4,10)]

boxplot(Litter~Site.Treatment.Time, data=Vik.litter, main="Litter above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#############Hogsete#######################################

#Boxplots for the graminoids in the tempo plots in Hogsete:

Functional.Temporary<-read.csv("Functional_Group_Biomass_Temporary_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

Hog.gram<-Functional.Temporary[c(19:36),c(4:5)]

boxplot(Graminoids~Site.Treatment.Time, data=Hog.gram, main="Graminoid above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the forbs in the tempo plots in Hogsete:

Hog.forb<-Functional.Temporary[c(19:36),c(4,6)]

boxplot(Forbs~Site.Treatment.Time, data=Hog.forb, main="Forb above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Bryophytes in the tempo plots in Hogsete:

Hog.bryo<-Functional.Temporary[c(19:36),c(4,7)]

boxplot(Bryophytes~Site.Treatment.Time, data=Hog.bryo, main="Bryophyte above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Lichen in the tempo plots in Hogsete:

Hog.lich<-Functional.Temporary[c(19:36),c(4,8)]

boxplot(Lichen~Site.Treatment.Time, data=Hog.lich, main="Lichen above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for Shrubs in the tempo plots in Hogsete:

Hog.shrub<-Functional.Temporary[c(19:36),c(4,9)]

boxplot(Shrubs~Site.Treatment.Time, data=Hog.shrub, main="Shrub above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Litter in the tempo plots in Hogsete:

Hog.litter<-Functional.Temporary[c(19:36),c(4,10)]

boxplot(Litter~Site.Treatment.Time, data=Hog.litter, main="Litter above-ground biomass in temporary plots in Hogsete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#############Joesete#######################################

#Boxplots for the graminoids in the tempo plots in Joesete:

Functional.Temporary<-read.csv("Functional_Group_Biomass_Temporary_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

Joe.gram<-Functional.Temporary[c(37:48),c(4:5)]

boxplot(Graminoids~Site.Treatment.Time, data=Joe.gram, main="Graminoid above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the forbs in the tempo plots in Joesete:

Joe.forb<-Functional.Temporary[c(37:48),c(4,6)]

boxplot(Forbs~Site.Treatment.Time, data=Joe.forb, main="Forb above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Bryophytes in the tempo plots in Joesete:

Joe.bryo<-Functional.Temporary[c(37:48),c(4,7)]

boxplot(Bryophytes~Site.Treatment.Time, data=Joe.bryo, main="Bryophyte above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Lichen in the tempo plots in Joesete:

Joe.lich<-Functional.Temporary[c(37:48),c(4,8)]

boxplot(Lichen~Site.Treatment.Time, data=Joe.lich, main="Lichen above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for Shrubs in the tempo plots in Joesete:

Joe.shrub<-Functional.Temporary[c(37:48),c(4,9)]

boxplot(Shrubs~Site.Treatment.Time, data=Joe.shrub, main="Shrub above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Litter in the tempo plots in Joesete:

Joe.litter<-Functional.Temporary[c(37:48),c(4,10)]

boxplot(Litter~Site.Treatment.Time, data=Joe.litter, main="Litter above-ground biomass in temporary plots in Joesete", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#############In Between#######################################

#Boxplots for the graminoids in the tempo plots in Between:

Functional.Temporary<-read.csv("Functional_Group_Biomass_Temporary_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

Bet.gram<-Functional.Temporary[c(49:54),c(4:5)]

boxplot(Graminoids~Site.Treatment.Time, data=Bet.gram, main="Graminoid above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the forbs in the tempo plots in Between:

Bet.forb<-Functional.Temporary[c(49:54),c(4,6)]

boxplot(Forbs~Site.Treatment.Time, data=Bet.forb, main="Forb above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Bryophytes in the tempo plots in Between:

Bet.bryo<-Functional.Temporary[c(49:54),c(4,7)]

boxplot(Bryophytes~Site.Treatment.Time, data=Bet.bryo, main="Bryophyte above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Lichen in the tempo plots in Between:

Bet.lich<-Functional.Temporary[c(49:54),c(4,8)]

boxplot(Lichen~Site.Treatment.Time, data=Bet.lich, main="Lichen above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for Shrubs in the tempo plots in Between:

Bet.shrub<-Functional.Temporary[c(49:54),c(4,9)]

boxplot(Shrubs~Site.Treatment.Time, data=Bet.shrub, main="Shrub above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Litter in the tempo plots in Between:

Bet.litter<-Functional.Temporary[c(49:54),c(4,10)]

boxplot(Litter~Site.Treatment.Time, data=Bet.litter, main="Litter above-ground biomass in temporary plots in Between", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#############Liahovden#######################################

#Boxplots for the graminoids in the tempo plots in Liahovden:

Functional.Temporary<-read.csv("Functional_Group_Biomass_Temporary_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

Lia.gram<-Functional.Temporary[c(55:66),c(4:5)]

boxplot(Graminoids~Site.Treatment.Time, data=Lia.gram, main="Graminoid above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the forbs in the tempo plots in Liahovden:

Lia.forb<-Functional.Temporary[c(55:66),c(4,6)]

boxplot(Forbs~Site.Treatment.Time, data=Lia.forb, main="Forb above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Bryophytes in the tempo plots in Liahovden:

Lia.bryo<-Functional.Temporary[c(55:66),c(4,7)]

boxplot(Bryophytes~Site.Treatment.Time, data=Lia.bryo, main="Bryophyte above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Lichen in the tempo plots in Liahovden:

Lia.lich<-Functional.Temporary[c(55:66),c(4,8)]

boxplot(Lichen~Site.Treatment.Time, data=Lia.lich, main="Lichen above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for Shrubs in the tempo plots in Liahovden:

Lia.shrub<-Functional.Temporary[c(55:66),c(4,9)]

boxplot(Shrubs~Site.Treatment.Time, data=Lia.shrub, main="Shrub above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

#Boxplots for the Litter in the tempo plots in Liahovden:

Lia.litter<-Functional.Temporary[c(55:66),c(4,10)]

boxplot(Litter~Site.Treatment.Time, data=Lia.litter, main="Litter above-ground biomass in temporary plots in Liahovden", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

