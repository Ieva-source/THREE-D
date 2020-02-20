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
                 labels = c("V", "H", "J", "B", "L"))

p10 <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot()
p10

p10 <- ggplot(F, aes(x = Site, y = Gperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Graminoids") + scale_fill_brewer(palette = "Accent") + theme_bw() +
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
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Forbs") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pF

pB <- ggplot(F, aes(x = Site, y = Bperday, fill = Treatment)) +
  geom_boxplot()
pB

pB <- ggplot(F, aes(x = Site, y = Bperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Bryophytes") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pB

pL <- ggplot(F, aes(x = Site, y = Lperday, fill = Treatment)) +
  geom_boxplot()
pL

pL <- ggplot(F, aes(x = Site, y = Lperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Lichen") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pL

data(F)
F$Site <- factor(F$Site,
                 labels = c("V", "H", "J", "B", "L"))

pS <- ggplot(F, aes(x = Site, y = Sperday, fill = Treatment)) +
  geom_boxplot()
pS

pS <- ggplot(F, aes(x = Site, y = Sperday, fill = Treatment)) +
  geom_boxplot(notch = FALSE) + scale_x_discrete(name = "Study sites") +
  scale_y_continuous(name = "Biomass/day (g)") + ggtitle("Shrubs") + scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Treatment")
pS

######################################################################
####Attempt to put multiple plots together############################
######################################################################

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

install.packages("ggpubr")
library(ggpubr)

ggarrange(p10, pF, pB, pL, pS + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2)
