##Trying bar plots##
###Too many species so it seems on the x axis and then they are doupled##
##Maybe good for functional groups

Hog<-read.csv("Hog_species_aver.csv",  
              sep=";",                        
              dec=",",                        
              header=TRUE,                    
              stringsAsFactors = FALSE) 

library(ggpubr)

# Convert the treatment variable to a factor
Hog$Treatment <- as.factor(Hog$Treatment)
# Add the name colums
Hog$Species <- columnnames(Hog)
# Inspect the data
head(Hog[, c("Species", "Biomass", "Treatment")])

ggbarplot(Hog, x = "Species", y = "Biomass",
          fill = "Treatment",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",          # Sort the value in dscending order
          sort.by.groups = TRUE,     # Don't sort inside each group
          x.text.angle = 90,           # Rotate vertically x axis texts
          ggtheme = theme_pubclean()
)+
  font("x.text", size = 4, vjust = 0.1)

###This is from the example###

dfm <- mtcars
# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)
# Add the name colums
dfm$name <- rownames(dfm)
# Inspect the data
head(dfm[, c("name", "wt", "mpg", "cyl")])

###Ploting the temperature data##

weather <- lincoln_weather

ggplot(
  lincoln_weather, 
  aes(x = `Mean Temperature [F]`, y = `Month`)
) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 3, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Temp. [F]"
  )+
  labs(title = 'Temperatures in Lincoln NE') 

###With my data##

Temp<-read.csv("Temperature_data_updated.csv",  
              sep=";",                        
              dec=",",                        
              header=TRUE,                    
              stringsAsFactors = FALSE) 

ggplot(
  Temp, 
  aes(x = `Temperature`, y = `Month`)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 0.6, size = 0.3
  ) +
  scale_fill_gradientn(
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
    name = "Temp. [C]"
  )+
  labs(title = 'Temperatures in C') 

ggplot(Temp, aes(x = Temperature, y = Month)) +
  geom_density_ridges(aes(fill = Month)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) 

###Trying the waffle plot for the species biomass in Hogsete###
###############################################################
##This wasn't working####################################
Hog<-read.csv("Hog.csv",  
              sep=";",                        
              dec=",",                        
              header=TRUE,                    
              stringsAsFactors = FALSE) 

install.packages("waffle")
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
library(waffle)


Hbiomas <- c('F.rub	(3,78)'=21, 'F.ovi	(1,78)' = 10, 'C.lep	(1,723)'= 9, 'D.ces	(1,538)'=8, 'Ant.odo	(1,426)' =8, 'Vac. Vitis-idea	(1,157)'=6, 'Pot.ere	(0,96)'=5, 'Agr.cap	(0,741)'= 4, 'V.off	(0,677)'= 4, 'Hyp.mac	(0,635)'=3, 'T.rep	(0,582)'=3, 'V.pal	(0,538)'=3, 'G.uli	(0,4)'=2, 'Alch.sp.	(0,395)'=2, 'Ach.mill	(0,36)'=2, 'C.sp.	(0,32)'=2, 'Phl.alp	(0,301)'=2, 'V.cha	(0,258)'=1, 
             'P.pra	(0,211)'=1, 'V.riv	(0,198)'=1, 'L.mult	(0,187)'=1, 'Others	(0,952)'=1)

H<- waffle(Hbiomas, rows=10, colors = rainbow(28),
       title="Mean biomass of plant species in Hogsete", 
       xlab="1 square = 0,001 g.") 

H + coord_flip()

parts <- c("F.rubra" = 21, "F.ovina" = 10, "C. leporina" = 9 , "D. cespitosa" = 8, "Ant. odoratum" = 8, "Vac. Vitis-idea" = 6, "Pot.ere	(0,96)"=5, "Agr.cap	(0,741)"= 4, "V.off	(0,677)"= 4, "Hyp.mac	(0,635)"=3, "T.rep	(0,582)"=3, "V.pal	(0,538)"=3, "G.uli	(0,4)"=2, "Alch.sp.	(0,395)"=2, "Ach.mill	(0,36)"=2, "C.sp.	(0,32)"=2, "Phl.alp	(0,301)"=2, "V.cha	(0,258)"=1,
           "P.pra	(0,211)"=1, "V.riv	(0,198)"=1, "L.mult	(0,187)"=1,"Others" = 5 + 4 + 4 + 3 + 3 + 3)
waffle(Hbiomas,
       rows=10,
       title = "Species biomass in Hogsete",
       xlab = "1 square ~ 1 % of total biomass",
       colors=c("#CC0000", "#006600", "#669999", "#00CCCC", 
                "#FF9999", "#FF9900","brown2","darkgoldenrod", "darkmagenta",
                "darkslategray1", "deeppink", "dimgray", "firebrick1", "forestgreen", 
                "gold1", "gray0", "hotpink4", "mistyrose", "orangered", "paleturquoise", "palevioletred4", "blue"))