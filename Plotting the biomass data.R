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
#########################in permanent plots##################

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

#############################################################
##############Boxplots for functional groups#################
#########################in temporary plots##################

#############Vikesland#######################################

#Boxplots for the graminoids in the tempo plots in Vikesland:

Functional.Temporary<-read.csv("Functional_Group_Biomass_Temporary_Plots.csv",  
                               sep=";",                        
                               dec=",",                        
                               header=TRUE,                    
                               stringsAsFactors = FALSE)

Vik.gram<-Functional.Temporary[c(1:18),c(4:5)]

boxplot(Graminoids~Site.Treatment.Time, data=Vik.gram, main="Graminoid above-ground biomass in temporary plots in Vikesland", xlab="Treatments and time", ylab="Total above-ground biomass (g)")

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

