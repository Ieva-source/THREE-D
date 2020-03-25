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


Vbiomass <- c('Deschampsia cespitosa	(6,080)'=	31,'Poa pratensis	(4,093)'=	21,'Anthoxanthum odoratum	(3,984)' =	20,'Rubus idaeus	(1,130)'=	6,
              'Galium uliginosum	(1,050)'=	5, 'Avenella flexuosa	(0,877)'=	4, 'Achillea millefolium	(0,846)'=	4,'Campanula rotundifolia	(0,846)'=	4,
              'Trifolium repens	(0,295)'=	1, 'Rumex acetosa	(0,287)'=	1,'Pimpinella saxifraga	(0,186)'=	1,'Others (0,251)' = 1)
         
V<- waffle(Vbiomass,
                 rows=10,
                 title = "A. Vikesland",
                 xlab = "1 square ~ 1 % of total biomass",
                 colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                          "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                          "chartreuse", "deeppink", "blue"))     

V

Hbiomas <- c('Festuca rubra	(3,78)'=21, 'Festuca ovina	(1,78)' = 10, 'Carex leporina	(1,723)'= 9, 'Deschampsia cespitosa	(1,538)'=8, 
             'Anthoxanthum odoratum	(1,426)' =8, 'Vaccinium vitis-idaea	(1,157)'=6, 'Potentilla erecta	(0,96)'=5, 'Agrostis capillaris	(0,741)'= 4, 
             'Veronica officinalis	(0,677)'= 4, 'Hypericum maculatum	(0,635)'=3, 'Trifolium repens	(0,582)'=3, 'Viola palustris	(0,538)'=3, 'Galium uliginosum	(0,4)'=2, 
             'Alchemilla sp.	(0,395)'=2, 'Achillea millefolium	(0,36)'=2, 'Carex sp.	(0,32)'=2, 'Phleum alpinum	(0,301)'=2, 'Veronica chamaedrys	(0,258)'=1, 
             'Poa pratensis	(0,211)'=1, 'Viola riviana	(0,198)'=1, 'Luzula multiflora	(0,187)'=1, 'Others	(0,952)'=1)


H<- waffle(Hbiomas,
       rows=10,
       title = "B. Høgsete",
       xlab = "1 square ~ 1 % of total biomass",
       colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                "chartreuse", "deeppink", "dimgray", "firebrick1", "forestgreen", 
                "slateblue1", "gray0", "hotpink4", "darkcyan", "orangered", "darkmagenta", "palevioletred4", "blue"))

H

Jbiomass <- c('Empetrum nigrum	(4,165)'=	21,'Avenella flexuosa	(3,68)'=	18,'Agrostis capillaris	(2,07)'=	10,'Anthoxanthum odoratum	(1,82)'=	9,'Deschampsia cespitosa	(1,197)'=	6,'Vaccinium vitis-idaea =	(1,102)'=	5,'Rumex acetosa	(1,025)'=	5,'Vaccinium myrtillus	(0,99)' =	5,
              'Poa sp.	(0,94)'=	5,'Alchemilla sp.	(0,935)'=	5,'Poa pratensis	(0,57)'=	3, 'Carex bigelowii	(0,47)'=	2,'Trientalis europaea	(0,3475)'=	2,'Phleum alpinum	(0,25)'=	1,'Carex sp. 	(0,24)'=	1,'Hypericum maculatum	(0,15)'=	1,'Luzula multiflora	(0,1055)'=	1,'Other (0,09)'=	1)

J <- waffle(Jbiomass,
            rows=10,
            title = "C. Joasete",
            xlab = "1 square ~ 1 % of total biomass",
            colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                     "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                     "chartreuse", "deeppink", "dimgray", "firebrick1", "forestgreen", 
                     "slateblue1", "gray0", "hotpink4", "blue"))
J

Bbiomass <- c('Nardus stricta	(8,745)'=	36,
              'Vaccinium uliginosum	(3,010)'=12,
              'Carex bigelowii	(1,330)'=	5,
              'Festuca ovina	(1,043)'=	4,
              'Anthoxanthum odoratum	(0,960)'=	4,
              'Trichophorum cespitosum	(0,960)'=	4,
              'Poa alpna	(0,950)'=	4,
              'Carex capillaris	(0,925)'=	4,
              'Deschampsia cespitosa	(0,805)'=	3,
              'Saxifraga aizoid	(0,770)'=	3,
              'Ccarex bigelowii	(0,710)'=	3,
              'Ranunculus repens	(0,580)'=	2,
              'Alchemilla sp.	(0,790)'=	3,
              'Parnassia palustris	(0,440)'=	2,
              'Thalictrum alpinum	(0,360)'=	1,
              'Bistorta vivipara	(0,350)'=	1,
              'Leontodon autumnalis	(0,338)'=	1,
              'Poa pratensis	(0,310)'=	1,
              'Ranunculus acris	(0,297)'=	1,
              'Taraxacum sp.	(0,235)'=	1,
              'Carex vaginate	(0,130)'=	1,
              'Euphrasia sp.	(0,130)'=	1,
              'Other	(0,119)'=	1)

B <- waffle(Bbiomass,
            rows=10,
            title = "D. In between",
            xlab = "1 square ~ 1 % of total biomass",
            colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                     "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                     "chartreuse", "deeppink", "dimgray", "firebrick1", "forestgreen", 
                     "slateblue1", "gray0", "hotpink4", "darkcyan", "orangered", "darkmagenta", "palevioletred4", "red", "chartreuse1", "blue"))
B
#(I can delete one collor from the B)

Lbiomass <- c('Saxifraga aizoides	(2,150)'=	23,
              'Carex sp. 	(0,883)'=	10,
              'Vaccinium myrtillus	(0,750)'=	8,
              'Salix herbacea	(0,655)'=	7,
              'Salix reticulata	(0,643)'=	7,
              'Festuca rubra	(0,638)'=	7,
              'Carex bigelowii	(0,540)'=	6,
              'Thalictrum alpinum	(0,480)'=	5,
              'Carex capillaris	(0,418)'=	5,
              'Bistorta vivipara	(0,343)'=	4,
              'Empetrum nigrum	(0,314)'=	3,
              'Poa alpina	(0,249)'=	3,
              'Solidago virgaurea	(0,215)'=	2,
              'Carex nigra	(0,201)'=	2,
              'Parnassia palustris	(0,200)'=	2,
              'Hypericum maculatum	(0,150)'=	2,
              'Saxifraga oppositifolia	(0,140)'=	2,
              'Deschampsia cespitosa	(0,073)'=	1,
              'Anthoxanthum odoratum	(0,047)'=	1,
              'Others	(0,086)'=	1)

L <- waffle(Lbiomass,
            rows=10,
            title = "E. Liahovden",
            xlab = "1 square ~ 1 % of total biomass",
            colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                     "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                     "chocolate1", "chartreuse", "dimgray", "firebrick1", "forestgreen", 
                     "slateblue1", "gray0", "hotpink4", "darkcyan", "orangered", "blue"))
L

install.packages("ggpubr")
library(ggpubr)

figure <- ggarrange(V, 
                    labels = c("A"),
                    ncol = 1, nrow = 1, commong.legend = FALSE)

figure

annotate_figure(figure,
                top = text_grob("Mean species biomass across the study sites", 
                                color = "black", face = "bold", size = 14))

##The same but with grazed plants###

#Vikesland##

Vdamage<- c("Poa pratensis"=	10,
      "Anthoxanthum odoratum"=	7,
      "Deschampsia cespitosa"=	5,
      "Agrostis capillaris"=	3,
      "Pimpinella saxifraga"=	3,
      "Veronica chamaedrys"=	2,
      "Rumex acetosa"=	2,
      "Stellaria gramminea"=	2,
      "Hypericm maculatum"=	2,
      "Phleum alpinum"=	1,
      "Achillea millefolium"=	1,
      "Ranunculus acris"=	1,
      "Trifolium repens"=	1,
      "Knautia arvensis"=	1)

vd <- waffle(Vdamage,
            rows=3,
            title = "Grazed plant species in Vikesland",
            xlab = "1 square = 1 time grazed",
            legend_pos = ("bottom"),
            colors=c("#CC0000", "#006600", "#669999", "darkgoldenrod", 
                     "#FF9999", "yellow","brown2","#00CCCC", "darkmagenta",
                     "chocolate1", "chartreuse", "dimgray", "firebrick1", "blue"))
vd

##Code from the website##

H<- waffle(Hbiomas, rows=10, colors = rainbow(28),
           title="Mean biomass of plant species in Hogsete", 
           xlab="1 square = 0,001 g.") 

H + coord_flip()

parts <- c("F.rubra" = 21, "F.ovina" = 10, "C. leporina" = 9 , "D. cespitosa" = 8, "Ant. odoratum" = 8, "Vac. Vitis-idea" = 6, "Pot.ere	(0,96)"=5, "Agr.cap	(0,741)"= 4, "V.off	(0,677)"= 4, "Hyp.mac	(0,635)"=3, "T.rep	(0,582)"=3, "V.pal	(0,538)"=3, "G.uli	(0,4)"=2, "Alch.sp.	(0,395)"=2, "Ach.mill	(0,36)"=2, "C.sp.	(0,32)"=2, "Phl.alp	(0,301)"=2, "V.cha	(0,258)"=1,
           "P.pra	(0,211)"=1, "V.riv	(0,198)"=1, "L.mult	(0,187)"=1,"Others" = 5 + 4 + 4 + 3 + 3 + 3)
