###Testing for a treament (grazing) effect in permanent plots############
########One-Way ANOVA####################################################

Testing<-read.csv("Testing_for_treatment_effect.csv",  
                    sep=";",                        
                    dec=",",                        
                    header=TRUE,                    
                    stringsAsFactors = FALSE)      

attach(Testing)
is.factor(Treatment)


boxplot(Biomass~Treatment, data=Testing, main="Biomass in caged and open permanent plots", xlab="Treatment", ylab="Total above-ground biomass (g)",
        col=rainbow(4))

model1<-aov(Biomass~Treatment)
summary (model1)

par(mfrow=c(2,2))
plot(model1)

#####Shapiro-Wilk test (testing for normality)####

uhat<-resid(model1)
shapiro.test(uhat)

#####Testing for homogeneity (bartlett test)####

bartlett.test(Biomass~Treatment)
