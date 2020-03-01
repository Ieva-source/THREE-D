###Testing if treament effect varies among sites in permanent plots######
########Two-Way ANOVA####################################################

P<-read.csv("Total_Biomass_Permanent_Plots.csv",  
                  sep=";",                        
                  dec=",",                        
                  header=TRUE,                    
                  stringsAsFactors = FALSE)     

attach(P)

summary(P)

str(P)

View(P)

########################################
###Testing for the main effect BIO/DAY##

Model_1<-aov(bioperday~Treatment+elevation, data=P)

summary(Model_1)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_1)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat1<-resid(Model_1)
shapiro.test(uhat1)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(bioperday~Treatment)

########################################
###Testing for the main effect BIO/DAY + 32 added to "In between"##

Model_3<-aov(bioperday32~Treatment+elevation, data=P)

summary(Model_3)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_3)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat3<-resid(Model_3)
shapiro.test(uhat3)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(bioperday32~Site)

########################################
###Testing for the interaction effect###

Model_2<-aov(bioperday~Treatment*elevation, data=P)

summary(Model_2)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_2)

#####Shapiro-Wilk test (testing for normality)####

uhat2<-resid(Model_2)
shapiro.test(uhat2)

########################################
###Testing for the interaction effect +32 days added to the "In between" site###

Model_4<-aov(bioperday32~Treatment*Sitename, data=P)

summary(Model_4)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_4)

#####Shapiro-Wilk test (testing for normality)####

uhat4<-resid(Model_4)
shapiro.test(uhat4)

####Not sure how to run the barlett test for this one###

bartlett.test(bioperday32~Site)

###Post-hoc test: Since we found a significant interaction effect###

library("lsmeans")
library("multcompView")

###Pairwise for Sites##

posthoc <-lsmeans(Model_4,
                  pairwise~Sitename,
                  adjust = "tukey")
posthoc

cld(posthoc,
    alpha=0.05,
    Letter=letters)

TukeyHSD(Model_4, "Sitename")

###Pairwise for Sites x Treatment##

posthoc <-lsmeans(Model_4,
                  pairwise~Treatment*Sitename,
                  adjust = "tukey")
posthoc

