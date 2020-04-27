Cov<-read.csv("Total_covers_all_sites.csv",              
              sep=";",                        
              dec=",",                        
              header=TRUE,                  
              stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(janeaustenr)

#Boxplots for the forbs in the tempo plots in Vikesland:

ViktoJoa<-Cov[c(1:104),c(1:3)]

ViktoJoa$Site <- factor(ViktoJoa$Site,
                   labels = c("A. Vikesland (469m)", "B. Høgsete (700m)", "C. Joasete (900m)"))
ViktoJoaa <- ViktoJoa %>%
  ggplot(aes(x = Species, y = Cover, fill = Site)) +
  geom_bar(stat="identity") +
  scale_alpha_manual(values = c(0.6, 1)) +
  labs(x= "", y = "Average cover %") +
  facet_wrap(~ Site, nrow = 5, ncol= 1, scales = "free") + theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 9, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 65, hjust =1, size = 8),
        legend.position = "none")
ViktoJoaa

BettoLia<-Cov[c(105:175),c(1:3)]

BettoLia$Site <- factor(BettoLia$Site,
                        labels = c("D. In between (1100m)", "E. Liahovden (1300m)"))

BettoLiaa <- BettoLia %>%
  ggplot(aes(x = Species, y = Cover, fill = Site)) +
  geom_bar(stat="identity") +
  scale_alpha_manual(values = c(0.6, 1)) +
  labs(x="", y = "Average cover %") +
  facet_wrap(~ Site, nrow = 5, ncol= 1, scales = "free") + theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 9, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 65, hjust =1, size = 8),
        legend.position = "none")
BettoLiaa

Cov$Site <- factor(Cov$Site,
                   labels = c("Vikesland (469m)", "Høgsete (700m)", "Joasete (900m)", "In between (1100m)", "Liahovden (1300m)"))

Cover <- Cov %>%
  ggplot(aes(x = Species, y = Cover, fill = Site)) +
  geom_bar(stat="identity") +
  scale_alpha_manual(values = c(0.6, 1)) +
  labs(x= "none", y = "Average cover %") +
  facet_wrap(~ Site, nrow = 5, ncol= 1, scales = "free") + theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 9, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(angle = 65, hjust =1, size = 8),
        legend.position = "none")
Cover
