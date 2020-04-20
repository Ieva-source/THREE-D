################################################################
####Let's look into the functional groups in temporary plots####
################################################################

TV<-read.csv("Biomas_Temporary_Vikesland.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)


library(ggplot2)
library(GGally)
library(dplyr)
library(tidyverse)
library(ggpubr)

attach(F)
TV$Harvest.nr <- factor(TV$Harvest.nr,
                 labels = c("June", "June", "July", "July", "August", "August"))

figure <- TV %>%
  pivot_longer(cols = c(-Treatment, -Site, -Plot.ID, -Treatment.Time, -Harvest.nr, -Month), names_to = "FunctionalGroup", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(FunctionalGroup = recode(FunctionalGroup, "Gperday" = "Graminoids", "Fperday" = "Forbs","Bperday" = "Bryophytes", "Lperday" = "Lichens", "Sperday" = "Shurbs", "bioperday" = "Total biomass")) %>%
  ggplot(aes(x = Harvest.nr, y = Value, fill = Treatment)) +
  geom_boxplot() +
  geom_title = "Biomass grown per day in Vikesland" +
  labs(x = "Harvest time", y = "Biomass (g)") +
  facet_wrap(~ FunctionalGroup, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")

figure

###Total biomass data from all tempo plots###

Total<-read.csv("Total_biomass_tempo_plots.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)

Total$Timestep <- factor(Total$Timestep,
                        labels = c("June", "July", "August"))

Totaltempo <- Total %>%
  pivot_longer(cols = c(-Treatment, -Timestep, -Harvest.time), names_to = "Studysites", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(Studysites = recode(Studysites, "A" = "469m(Vikesland)", "C" = "700m(Høgsete)","B" = "900m(Joasete)", "D" = "91300m(Liahovden)")) %>%
  ggplot(aes(x = Timestep, y = Value, fill = Treatment)) +
  geom_boxplot()+
  labs(x = "Month", y = "Biomass per day (g)") +
  facet_wrap(~ Studysites, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")
Totaltempo

annotate_figure(Totaltempo,
                top = text_grob("Total biomass grown per day in temporary plots", 
                                color = "black", face = "bold", size = 13))

Model_14<-aov(A~Treatment*Timestep, data=Total)

summary(Model_14)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_14)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat14<-resid(Model_14)
shapiro.test(uhat14)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(A~Treatment, data=Total)


###Data from all tempo plots from all functional groups###

Tempo<-read.csv("Func_group_tempo_plots_biomass.csv",  
                sep=";",                        
                dec=",",                        
                header=TRUE,                    
                stringsAsFactors = FALSE)

###Stacked barplot###

# library
library(ggplot2)

Tempo$Site <- factor(Tempo$Site,
                         labels = c("V", "H", "J", "L"))

Tempo$Month <- factor(Tempo$Month,
                     labels = c("June Control", "July Control", "August Control", "June Cage", "July Cage", "August Cage"))

bar <- Tempo %>%
  pivot_longer(cols = c(-Site, -Month), names_to = "FunctionalGroup", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(FunctionalGroup = recode(FunctionalGroup, "1A" = "Graminoids", "2B" = "Forbs","3C" = "Bryophytes", "4D" = "Lichens", "5E" = "Shurbs", "6F" = "Litter")) %>%
  ggplot(aes(x = Site, y = Value, fill = FunctionalGroup, fill=color)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(name= "Func. group", values=c("darkgreen", "red", "gold", "dodgerblue", "deeppink", "darkgrey"), labels = c("Graminoids", "Forbs", "Bryophytes", "Lichen", "Shrub", "Litter"))+
  labs(x="Site", y = "Biomass/day (g)") +
  facet_wrap(~ Month) + theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "right")
bar


annotate_figure(Alltempo,
                top = text_grob("Biomass grown per day of functional groups and litter in temporary plots", 
                                color = "black", face = "bold", size = 13))

####Three-way ANOVA###

##############################################################
###Testing if the functional groups differ in grazing effect##

Model_8<-aov(Bioperday~Treatment*Site*Harvest.time, data=Tempo)

summary(Model_8)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_8)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat8<-resid(Model_8)
shapiro.test(uhat8)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bioperday~Site, data=Tempo)

###For graminoids####

Model_9<-aov(log10(Graminoids)~Treatment*Site*Harvest.time, data=Tempo)

summary(Model_9)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_9)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat9<-resid(Model_9)
shapiro.test(uhat9)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Graminoids~Site, data=Tempo)

###For forbs####

Model_10<-aov(Forbs~Treatment*Site*Harvest.time, data=Tempo)

summary(Model_10)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_10)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat10<-resid(Model_10)
shapiro.test(uhat10)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Forbs~Site, data=Tempo)

###For bryophytes####

Model_11<-aov(log10(Bryophytes)~Treatment*Site*Harvest.time, data=Tempo)

summary(Model_11)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_11)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat11<-resid(Model_11)
shapiro.test(uhat11)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bryophytes~Site, data=Tempo)

###For lichen###

Model_12<-aov(log10(Lichen)~Treatment*Site*Harvest.time, data=Tempo)

summary(Model_12)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_12)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat12<-resid(Model_12)
shapiro.test(uhat12)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Lichen~Site, data=Tempo)

#####For biomass of gramminoids, forbs and bryophytes#####

Model_13<-aov(log10(Biomass)~Treatment1*Site1*Harvest.time1*Group, data=Tempo)

summary(Model_13)

par(mfrow=c(2,2))

plot(Model_13)

uhat13<-resid(Model_13)
shapiro.test(uhat13)

