#########################################################################
###Testing for a treament (grazing) effect in permanent plots############
########One-Way ANOVA####################################################

A<-read.csv("Total_Biomass_Permanent_Plots.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)      

attach(A)
is.factor(A)

################################
###Looking into TOTAL BIOMASS###
################################


boxplot(Total.Biomass~Treatment, data=A, main="Biomass in caged and open permanent plots", xlab="Treatment", ylab="Total above-ground biomass (g)",
        col=rainbow(4))

model1<-aov(Total.Biomass~Treatment)
summary (model1)

par(mfrow=c(2,2))
plot(model1)

#####Shapiro-Wilk test (testing for normality)####

uhat<-resid(model1)
shapiro.test(uhat)

#####Testing for homogeneity (bartlett test)####

bartlett.test(Total.Biomass~Treatment)

#################################################
###########Looking into BIO/DAY data#############
#################################################

par(mfrow=c(1,1))

boxplot(bioperday~Treatment, data=A, main="Bio/day in caged and open permanent plots", xlab="Treatment", ylab="Biomass/day (g)",
        col=rainbow(4))

model2<-aov(bioperday~Treatment)
summary (model2)

par(mfrow=c(2,2))
plot(model2)

#####Shapiro-Wilk test (testing for normality)####

uhat2<-resid(model2)
shapiro.test(uhat2)

#####Testing for homogeneity (bartlett test)####

bartlett.test(bioperday~Treatment)

##################################################
###########Looking into BIO/DAY data #############
#####when the in between has + 32 days added######

A<-read.csv("Total_Biomass_Permanent_Plots.csv",  
            sep=";",                        
            dec=",",                        
            header=TRUE,                    
            stringsAsFactors = FALSE)  
attach(A)

par(mfrow=c(1,1))

boxplot(bioperday32~Treatment, data=A, main="Bio/day in caged and open permanent plots", xlab="Treatment", ylab="Biomass/day (g)",
        col=rainbow(4))

model3<-aov(bioperday32~Treatment)
summary (model3)

par(mfrow=c(2,2))
plot(model3)

#####Shapiro-Wilk test (testing for normality)####

uhat3<-resid(model3)
shapiro.test(uhat3)

#####Testing for homogeneity (bartlett test)####

bartlett.test(bioperday32~Treatment)