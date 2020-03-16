###################################################################################
######What I am using for the final presentation of the resuls#####################
###################################################################################

#Inspecting data visually######################################################
##Boxplots for the biomass data in permanent plots#############################

Permanent<-read.csv("Total_Biomass_Permanent_Plots.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

library(ggplot2)

data(Permanent)
Permanent$elevation <- factor(Permanent$elevation,
                              labels = c("Vikesland (469)", "Høgsete (700)", "Joasete (900)",
                                         "In Between (1100)", "Liahovden (1300)"))


pT <- ggplot(Permanent, aes(x = elevation, y = Total.Biomass, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites and elevation (a.s.l)") +
  scale_y_continuous(name = "Total above-ground biomass (g)") + ggtitle("Total above-ground biomass in permanent plots across all study sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pT

##########################################################################
###The code from Aud######################################################
##########################################################################

library("tidyverse")
dat <- read_csv2(file = "Final_biomass_permanent.csv")
figure <- dat %>%
  pivot_longer(cols = c(-Treatment, -Site), names_to = "Biomasspermanent", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(Biomasspermanent = recode(Biomasspermanent, "bioperday" = "A. Standardized to biomass/day", "Total Biomass" = "B. Total biomass")) %>%
  mutate(Elevation = case_when(Site == 1 ~ 469,
                               Site == 2 ~ 700,
                               Site == 3 ~ 900,
                               Site == 4 ~ 1100,
                               Site == 5 ~ 1300)) %>%
  ggplot(aes(x = factor(Elevation), y = Value, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Elevation (m)", y = "Above-ground biomass (g)") +
  facet_wrap(~ Biomasspermanent, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")

figure

library(ggpubr)


annotate_figure(figure,
                top = text_grob("Above-ground biomass in permanent plots", 
                                color = "black", face = "bold", size = 14))


##########################################################################
###The code from Aud NOT MODIFIED######################################################
##########################################################################

library("tidyverse")
dat <- read_csv2(file = "Func_Group_Perm_Plos_standardized.csv")
figure <- dat %>%
  pivot_longer(cols = c(-Treatment, -Site), names_to = "FunctionalGroup", values_to = "Value") %>%
  filter(Value != 0) %>%
  mutate(FunctionalGroup = recode(FunctionalGroup, "Fperday" = "Forbs", "Gperday" = "Graminoids","Bperday" = "Bryophytes", "Lperday" = "Lichens", "Sperday" = "Shurbs")) %>%
  mutate(Elevation = case_when(Site == 1 ~ 469,
                               Site == 2 ~ 700,
                               Site == 3 ~ 920,
                               Site == 4 ~ 1100,
                               Site == 5 ~ 1290)) %>%
  ggplot(aes(x = factor(Elevation), y = Value, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Elevation (m)", y = "Biomass per day (g)") +
  facet_wrap(~ FunctionalGroup, scales = "free_y") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")+
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")

library(ggpubr)

figure

annotate_figure(figure,
                top = text_grob("Above-ground biomass of functional groups and litter in permanent plots", 
                                color = "black", face = "bold", size = 14))


########################################################################
##THREE-WAY ANOVA##
###Testing if the effect of grazing differ in different functional groups accros study sites######
########Two-Way ANOVA####################################################

F<-read.csv("3-way_data.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)

attach(F)

View(F)



##############################################################
###Testing if the functional groups differ in grazing effect##


Model_5<-aov(Bperday~Treatment*Site*Group, data=F)

summary(Model_5)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_5)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat5<-resid(Model_5)
shapiro.test(uhat5)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bperday~Group)