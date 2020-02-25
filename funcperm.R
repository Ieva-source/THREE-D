################################################################
####Let's look into the functional groups in permanent plots####
################################################################

F<-read.csv("Functional_Group_Biomass_Permanent_Plots.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)

library(ggplot2)
library(GGally)
library(dplyr)

attach(F)
F$Site <- factor(F$Site,
                 labels = c("469", "700", "920", "1100", "1290"))

#############################################################################
#############################################################################
##Graminoids##

pG <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot()
pG

pG <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Graminoids") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pG


##Forbs##

pF <- ggplot(F, aes(x = Site, y = Fperday, fill = Treatment)) +
  geom_boxplot()
pF

pF <- ggplot(F, aes(x = Site, y = Fperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Forbs") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pF

##Bryophytes##

pB <- ggplot(F, aes(x = Site, y = Bperday, fill = Treatment)) +
  geom_boxplot()
pB

pB <- ggplot(F, aes(x = Site, y = Bperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Bryophytes") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pB

##Lichen##

pL <- ggplot(F, aes(x = Site, y = Lperday, fill = Treatment)) +
  geom_boxplot()
pL

pL <- ggplot(F, aes(x = Site, y = Lperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Lichen") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pL


###Shrub##

pS <- ggplot(F, aes(x = Site, y = Sperday, fill = Treatment)) +
  geom_boxplot()
pS

pS <- ggplot(F, aes(x = Site, y = Sperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Shrubs") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pS

#######################################################################
##Litter per day#######################################################

pLitter <- ggplot(F, aes(x = Site, y = LitterStand, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Elevation") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Litter") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pLitter

#######################################################
####Multiple plots together############################
#######################################################

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

install.packages("ggpubr")
library(ggpubr)

figure <- ggarrange(pG, pF, pB, pS, pL, pLitter, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, common.legend = TRUE, 
          legend="bottom")

figure

annotate_figure(figure,
                top = text_grob("Biomass/day of functional groups in permanent plots", 
                                color = "black", face = "bold", size = 14))

##########################################################################
###The code from Aud######################################################
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
