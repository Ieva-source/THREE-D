########################################################
###########Soil Moisture data###########################
########################################################

M<-read.csv("Testing_soil_moisture_cage_no_cage.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)
library(tidyverse)
library(scales)
library(lsmeans)
library(ggpubr)

##########################################################################
###Testing for caged-indusedeffect###############################
##########################################################################

attach(M)
head(M)
par(mfrow=c(1,1))

soilmoisture <- M %>%
  pivot_longer(cols = c(-Date, -Sitename, -Site, -Elevation, -Treatment, -Logger.ID), names_to = "Temperature", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(Temperature = recode(Temperature, "Soil.Moisture" = "Cage vs no-cage soil moisture")) %>%
  mutate(Elevation = case_when(Site == 1 ~ 469,
                               Site == 2 ~ 900,
                               Site == 3 ~ 1300)) %>%
  ggplot(aes(x = factor(Elevation), y = Value, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Elevation (m)", y = "Volumetric soil moisture") +
  facet_wrap(~ Temperature, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")

soilmoisture

##Testing for possible cage-indused difference##
###Non-parametric alternative to One-way ANOVA###


boxplot(Soil.Moisture~Treatment, data=M, main="Soil moisture", xlab="Treatment", ylab="Soit temp",
        col=rainbow(4))

head(M)

levels(M$Treatment)

M$Treatment <- ordered(M$Treatment, 
                       levels = c("cage", "no-cage"))

##Summary statistics by groups##
group_by(M, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(Soil.Moisture, na.rm = TRUE),
    sd = sd(Soil.Moisture, na.rm = TRUE),
    median = median(Soil.Moisture, na.rm = TRUE),
    IQR = IQR(Soil.Moisture, na.rm = TRUE)
  )

kruskal.test(Soil.Moisture ~ Treatment, data = M)

citation(package = "ggplot2")

###Anova's just in case###
##########################################################
model1<-aov(log(Soil.Moisture)~Treatment, data=M)
summary (model1)

par(mfrow=c(2,2))
plot(model1)

#####Shapiro-Wilk test (testing for normality)####

uhat<-resid(model1)
shapiro.test(uhat)

#####Testing for homogeneity (bartlett test)####

bartlett.test(Soil.Moisture~Treatment, data=M)



#################################################################
###Average temperature over time#################################

monthly<-read.csv("Montly_soil_moisture.csv",  
               sep=";",                        
               dec=",",                        
               header=TRUE,                    
               stringsAsFactors = FALSE)


monthly$Date <- factor(monthly$Date, levels=c("July", "Aug", "Sep"))

monthly$Site <- factor(monthly$Site,
                    labels = c("Vikesland(469m)", "Høgsete(700m)", "Joasete(900m)", "In between(1100m)", "Liahovden(1300m)"))


Aver<- monthly %>%
  mutate(Date = factor(Date, levels = c("July", "Aug", "Sep"))) %>%
  mutate(Months = as.numeric(Date)) %>%
  pivot_longer(cols = c(-Date, -Months, -Site), names_to = "Temperature", values_to = "Value") %>%
  mutate(Temperature = recode(Temperature, "Soil.Moisture" = "Mean monthly volumetric soil moisture")) %>%
  ggplot(aes(x = Months, y = Value, fill = Site, color=Site)) +
  geom_point()+
  geom_line(aes(color=Site, size=Site))+
  scale_color_manual(values=c("green", "#006600", "#669999", "darkgoldenrod","#CC0000"))+
  scale_size_manual(values=c(0.9, 0.9, 0.9, 0.9, 0.9)) +
  labs(y = "Volumetric soil moisture") +
  facet_wrap(~ Temperature, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "none")+
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Jul", "Aug", "Sep")) +
  labs(fill = "Site")

Aver

##################################################################
###Tmst loggers only caged next to my plots#######################

Mymois<-read.csv("Daily_soil_moisture.csv",  
                 sep=";",                        
                 dec=",",                        
                 header=TRUE,                    
                 stringsAsFactors = FALSE)

str(Mymois)

Mymois$Date <- as.Date(Mymois$Date,
                       format = "%Y-%m-%d") # convert date column to date class

class(Mymois$Date) # view R class of data

head(Mymois$Date) # view results

Mymois$Site <- factor(Mymois$Site,
                      labels = c("Vikesland(469m)", "Høgsete(700m)", "Joasete(900m)", "In between(1100m)", "Liahovden(1300m)"))


P<- Mymois %>%
  pivot_longer(cols = c(-Date, -Sitename, -Elevation, -Site), names_to = "temperature", values_to = "Value") %>%
  mutate(Temperature = recode(temperature, "Soil.Moisture" = "Mean daily volumetric soil moisture")) %>%
  ggplot(aes(x = Date, y = Value, fill = Site, color=Site)) +
  geom_line(aes(color=Site, size=Site))+
  scale_color_manual(values=c("green", "#006600", "#669999", "darkgoldenrod","#CC0000"))+
  scale_size_manual(values=c(0.9, 0.9, 0.9, 0.9, 0.9))+
  labs(y = "Volumetric soil moisture ") +
  facet_wrap(~ Temperature, scales = "free_y") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 11, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "bottom")+
  scale_x_date(breaks = "1 month") +
  labs(fill = "Site")

P

##Testing if temperature is changing with elevation##

head(Mymois)

levels(Mymois$Site)


##Summary statistics by groups##
group_by(Mymois, Site) %>%
  summarise(
    count = n(),
    mean = mean(Soil.Moisture, na.rm = TRUE),
    sd = sd(Soil.Moisture, na.rm = TRUE),
    median = median(Soil.Moisture, na.rm = TRUE),
    IQR = IQR(Soil.Moisture, na.rm = TRUE)
  )

kruskal.test(Soil.Moisture ~ Site, data = Mymois)

pairwise.wilcox.test(Mymois$Soil.Moisture, Mymois$Site,
                     p.adjust.method = "BH")


citation(package = "ggplot2")

#####Data from the climate stations in Hogsete and Vikesland###

load(file = "Tempsoil_Ieva.Rdata", verbose = TRUE)

stations<- tempsoil

stations$Date <- as.Date(stations$Date,
                         format = "%Y-%m-%d") # convert date column to date class

class(stations$Date) # view R class of data


STA <- stations %>%
  ggplot(aes(x = Date, y = Temperature, fill = Sitename, color=Sitename)) +
  geom_line(aes(color=Sitename, size=Sitename))+
  scale_color_manual(values=c("#CC0000", "#006600"))+
  scale_size_manual(values=c(0.9, 0.9))+
  labs(y = "C°") +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "bottom")+
  scale_x_date(breaks = "2 months") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Sitename")

STA


###Climate plot###

library(ggpubr)

figure <- ggarrange(soilmoisture, Aver, P, 
                    labels = c("A", "B","C" ),
                    ncol = 2, nrow = 2, commong.legend = FALSE)

figure

figure2 <- ggarrange(P,                                                 # First row with scatter plot
                     ggarrange(soilmoisture, Aver, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
                     nrow = 2, 
                     labels = "A")                                       # Labels of the scatter plot) 

figure2

annotate_figure(figure,
                top = text_grob("Above-ground biomass of functional groups and litter in permanent plots", 
                                color = "black", face = "bold", size = 13))