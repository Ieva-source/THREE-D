########################################################################
###Testing if the effect of grazing differ in different functional groups accros study sites######
########Two-Way ANOVA####################################################

F<-read.csv("Functional_Group_Biomass_Permanent_Plots.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)

attach(F)

View(F)

##############################################################
###Testing if the functional groups differ in grazing effect##

##Graminoids##

Model_5<-aov(Bioperday~Treatment*Sitename*Gperday, data=F)

summary(Model_5)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_5)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat5<-resid(Model_5)
shapiro.test(uhat5)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bioperday~Gperday)

##GraminoidsTRANSFORMED DATA to log10##

Model_6<-aov(Bioperdaylog~Treatment*Sitename*Gperdaylog, data=F)

summary(Model_6)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_6)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat6<-resid(Model_6)
shapiro.test(uhat6)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bioperdaylog~Gperdaylog)

###########
###Forbs###

Model_7<-aov(Bioperdaylog~Treatment*Sitename*Fperdaylog, data=F)

summary(Model_7)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_7)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat7<-resid(Model_7)
shapiro.test(uhat7)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bioperdaylog~Fperdaylog)

###Bryophytes###

Model_8<-aov(Bioperdaylog~Treatment*Sitename*Bperdaylog, data=F)

summary(Model_8)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_8)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat8<-resid(Model_8)
shapiro.test(uhat8)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(Bioperdaylog~Fperdaylog)