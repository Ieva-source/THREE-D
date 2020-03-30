########################################################
###########Temperature data#############################
########################################################

T<-read.csv("Testing_cageindused_warming.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)


##########################################################################
###Testing for caged-indused warming effect###############################
##########################################################################

library("tidyverse")

warming <- T %>%
  pivot_longer(cols = c(-Date, -Sitename, -Site, -Elevation, -Treatment, -Logger.ID), names_to = "Temperature", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(Temperature = recode(Temperature, "AverSoilT" = "Soil", "AverGroundT" = "Ground","AverAirT" = "Air")) %>%
  mutate(Elevation = case_when(Site == 1 ~ 469,
                               Site == 2 ~ 900,
                               Site == 3 ~ 1100)) %>%
  ggplot(aes(x = factor(Elevation), y = Value, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Elevation (m)", y = "Temperature (C)") +
  facet_wrap(~ Temperature, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")

warming

#################################################################
###Average temperature over time#################################

data<-read.csv("Aver_Temp.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)


data$Date <- factor(data$Date, levels=c("July", "Aug", "Sep"))


line2 <- data %>%
  pivot_longer(cols = c(-Date, -Site,-seq), names_to = "Temperature", values_to = "Value") %>%
  mutate(Temperature = recode(Temperature, "SoilT" = "Soil", "GroundT" = "Ground","AirT" = "Air")) %>%
  ggplot(aes(x = Date, y = Value, fill = Site, color=Site)) +
  geom_point()+
  labs(x = "Date", y = "Temperature (C)") +
  facet_wrap(~ Temperature, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 12),
        legend.position = "right")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Site")
line2

library(ggpubr)

figure

annotate_figure(figure,
                top = text_grob("Above-ground biomass of functional groups and litter in permanent plots", 
                                color = "black", face = "bold", size = 13))
