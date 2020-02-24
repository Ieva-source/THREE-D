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

