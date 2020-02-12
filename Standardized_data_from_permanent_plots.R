###Comparing standardized data with not standardized###

Permanent<-read.csv("Total_Biomass_Permanent_Plots_Final.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

boxplot(Total.Biomass~Site, data=Permanent, main="Total biomass in permanent plots", xlab="Study sites and treatment", ylab="Above-ground biomass (g)", col=rainbow(2))

boxplot(bioperday~Site, data=Permanent, main="Biomass/day in permanent plots", xlab="Study sites and treatment", ylab="Biomass per day (g)", col=rainbow(2))

###One-way ANOVA to test for treament effect###

attach(Permanent)
is.factor(Treatment)

boxplot(bioperday~Treatment, data=Permanent, main="Biomass perday in caged vs open permanent plots", xlab="Treatment", ylab="Biomass/day (g)",
        col=rainbow(2))

model1<-aov(bioperday~Treatment)
summary (model1)

par(mfrow=c(2,2))
plot(model1)

#####Shapiro-Wilk test (testing for normality)####

uhat<-resid(model1)
shapiro.test(uhat)

#####Testing for homogeneity (bartlett test)####

bartlett.test(bioperday~Treatment)

###Two-way ANOVA to check if the treatment effect varies among sites###

summary(Permanent)

str(Permanent)

View(Permanent)

################################
###Testing for the main effect##

Model_1<-aov(bioperday~Treatment+elevation, data=Permanent)

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
###Testing for the interaction effect###

Model_2<-aov(bioperday~Treatment*elevation, data=Permanent)

summary(Model_2)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_2)

#####Shapiro-Wilk test (testing for normality)####

uhat2<-resid(Model_2)
shapiro.test(uhat2)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(bioperday~elevation)

###No significat interaction effect, just the treatment effect###

#################################################################
#################################################################
###The same analysis as above just without the "In Between" site#

Nobetween<-read.csv("Total_Biomass_Permanent_Plots_without_In Between.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

boxplot(Total.Biomass~Site, data=Nobetween, main="Total biomass in permanent plots without in Between", xlab="Study sites and treatment", ylab="Above-ground biomass (g)", col=rainbow(2))

boxplot(bioperday~Site, data=Nobetween, main="Biomass/day in permanent plots without in Between", xlab="Study sites and treatment", ylab="Biomass per day (g)", col=rainbow(2))

Permanent<-read.csv("Total_Biomass_Permanent_Plots_Final.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)

boxplot(Total.Biomass~Site, data=Permanent, main="Total biomass in permanent plots", xlab="Study sites and treatment", ylab="Above-ground biomass (g)", col=rainbow(2))

boxplot(bioperday~Site, data=Permanent, main="Biomass/day in permanent plots", xlab="Study sites and treatment", ylab="Biomass per day (g)", col=rainbow(2))

###One-way ANOVA to test for treament effect###

attach(Nobetween)
is.factor(Nobetween)

boxplot(bioperday~Treatment, data=Nobetween, main="Biomass perday in caged vs open permanent plots", xlab="Treatment", ylab="Biomass/day (g)",
        col=rainbow(2))

model1<-aov(bioperday~Treatment)
summary (model1)

par(mfrow=c(2,2))
plot(model1)

#####Shapiro-Wilk test (testing for normality)####

uhat<-resid(model1)
shapiro.test(uhat)

#####Testing for homogeneity (bartlett test)####

bartlett.test(bioperday~Treatment)

###Two-way ANOVA to check if the treatment effect varies among sites###

summary(Nobetween)

str(Nobetween)

View(Nobetween)

################################
###Testing for the main effect##

Model_1<-aov(bioperday~Treatment+elevation, data=Nobetween)

summary(Model_1)

###Cheking if the model assumptions are met###

par(mfrow=c(2,2))

plot(Model_1)

#####Assumption 1: Normal distribution: Shapiro-Wilk test (testing for normality)####

uhat1<-resid(Model_1)
shapiro.test(uhat1)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(bioperday~elevation)

########################################
###Testing for the interaction effect###

Model_2<-aov(bioperday~Treatment*elevation, data=Nobetween)

summary(Model_2)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_2)

#####Shapiro-Wilk test (testing for normality)####

uhat2<-resid(Model_2)
shapiro.test(uhat2)

####Assumption 2: Homogeneity of variance of the groups###

bartlett.test(bioperday~Site)

