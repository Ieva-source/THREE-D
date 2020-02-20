########################################################################
###Trying the ggplot to nicely visualize the biomass data from permanent plots###
#################################################################################

Permanent<-read.csv("Total_Biomass_Permanent_Plots.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

library(ggplot2)

data(Permanent)
Permanent$elevation <- factor(Permanent$elevation,
                           labels = c("Vikesland", "Hogsete", "Joasete", "In Between", "Liahovden"))

p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday, fill = Treatment)) +
  geom_boxplot()
p10

###Customizing the labels###

p10 <- p10 + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)")
p10

###Adding the title###

p10 <- p10 + ggtitle("Biomass/perday in permanent plots across sites")
p10

p10 <- p10 + geom_jitter()
p10

p10 <- p10 + scale_fill_brewer(palette = "Accent")
p10

###Just boxolots across sites with different treatments###

p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Above-ground biomass grown per day in permanent plots across all study sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

###Adding notch to the boxplot###
###With notch and data dots###
p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday, fill = Treatment)) +
  geom_boxplot(notch = TRUE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Above-ground biomass grown per day in permanent plots across all study sites") + geom_jitter() + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

#########################################################################
###Same but with totalbiomass############################################

data(Permanent)
Permanent$elevation <- factor(Permanent$elevation,
                              labels = c("Vikesland", "Hogsete", "Joasete", "In Between", "Liahovden"))

p10 <- ggplot(Permanent, aes(x = elevation, y = Total.Biomass, fill = Treatment)) +
  geom_boxplot()
p10

###Customizing the labels###

p10 <- p10 + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Total above-ground biomass")
p10

###Adding the title###

p10 <- p10 + ggtitle("Total above-ground biomass in permanent plots across sites")
p10

p10 <- p10 + geom_jitter()
p10

p10 <- p10 + scale_fill_brewer(palette = "Accent")
p10

###Just boxolots across sites with different treatments###

p10 <- ggplot(Permanent, aes(x = elevation, y = Total.Biomass, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Total above-ground biomass (g)") + ggtitle("Total above-ground biomass in permanent plots across all study sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

###Adding notch to the boxplot###
###With notch and data dots###
p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday, fill = Treatment)) +
  geom_boxplot(notch = TRUE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Above-ground biomass grown per day in permanent plots across all study sites") + geom_jitter() + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

########################################################
####bioperday + 32 days for the In between site#########
########################################################

attach(Permanent)
data(Permanent)
Permanent$elevation <- factor(Permanent$elevation,
                              labels = c("Vikesland", "Hogsete", "Joasete", "In Between", "Liahovden"))

p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday32, fill = Treatment)) +
  geom_boxplot()
p10

###Customizing the labels###

p10 <- p10 + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day")
p10

###Adding the title###

p10 <- p10 + ggtitle("Above-ground biomass harvested per day in permanent plots across sites")
p10

p10 <- p10 + geom_jitter()
p10

p10 <- p10 + scale_fill_brewer(palette = "Accent")
p10

###Just boxolots across sites with different treatments###

p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday32, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Above-ground biomass harvested per day in permanent plots across sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

###Adding notch to the boxplot###
###With notch and data dots###
p10 <- ggplot(Permanent, aes(x = elevation, y = bioperday32, fill = Treatment)) +
  geom_boxplot(notch = TRUE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Above-ground biomass harvested per day in permanent plots across sites") + scale_fill_brewer(palette = "Accent") + theme_bw() + geom_jitter() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

################################################################
####Let's look into the functional groups in permanent plots####
################################################################

F<-read.csv("Functional_Group_Biomass_Permanent_Plots.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

library(ggplot2)

data(F)
F$Site <- factor(F$Site,
                              labels = c("Vikesland", "Hogsete", "Joasete", "In Between", "Liahovden"))

p10 <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot()
p10

p10 <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Graminoid above-ground biomass grown per day in permanent plots across sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
p10

pF <- ggplot(F, aes(x = Site, y = Fperday, fill = Treatment)) +
  geom_boxplot()
pF

pF <- ggplot(F, aes(x = Site, y = Fperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Forbs above-ground biomass grown per day in permanent plots across sites") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pF

######################################################################
####Attempt to put multiple plots together############################
######################################################################

install.packages("ggpubr")
library(ggpubr)

ggarrange(p10, pF + rremove("x.text"), 
          labels = c("Graminoids", "Forbs"),
          ncol = 1, nrow = 2)

install.packages("installr"); library(installr) # install+load installr

updateR()