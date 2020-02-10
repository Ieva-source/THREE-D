############################################################
###To test if the treatment effect changes during summer###
#####in different study sites##############################

###################Vikesland##############################

Testing3<-read.csv("Vikesland_treatment-time_effect.csv",  
                   sep=";",                        
                   dec=",",                        
                   header=TRUE,                    
                   stringsAsFactors = FALSE)     

attach(Testing3)

summary(Testing3)

str(Testing3)

View(Testing3)

Model_3<-aov(biomass~treatment*time, data=Testing3)

summary(Model_3)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_3)

#####Shapiro-Wilk test (testing for normality)####

uhat3<-resid(Model_3)
shapiro.test(uhat3)

####Assumption 2: Homogeneity of variance of the groups###

leveneTest(biomass~treatment*time, data=Testing3)

###################Hogsete##############################

Testing4<-read.csv("Hogsete_treatment-time_effect.csv",  
                   sep=";",                        
                   dec=",",                        
                   header=TRUE,                    
                   stringsAsFactors = FALSE)     

attach(Testing4)

summary(Testing4)

str(Testing4)

View(Testing4)

Model_4<-aov(biomass~treatment*time, data=Testing4)

summary(Model_4)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_4)

#####Shapiro-Wilk test (testing for normality)####

uhat4<-resid(Model_4)
shapiro.test(uhat4)

####Assumption 2: Homogeneity of variance of the groups###

leveneTest(biomass~treatment*time, data=Testing4)

###################Joasete##############################

Testing5<-read.csv("Joasete_treatment-time_effect.csv",  
                   sep=";",                        
                   dec=",",                        
                   header=TRUE,                    
                   stringsAsFactors = FALSE)     

attach(Testing5)

summary(Testing5)

str(Testing5)

View(Testing5)

Model_5<-aov(biomass~treatment*time, data=Testing5)

summary(Model_5)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_5)

#####Shapiro-Wilk test (testing for normality)####

uhat5<-resid(Model_5)
shapiro.test(uhat5)

####Assumption 2: Homogeneity of variance of the groups###

leveneTest(biomass~treatment*time, data=Testing5)

###################Liahovden##############################

Testing6<-read.csv("Liahovden_treament-time_effect.csv",  
                   sep=";",                        
                   dec=",",                        
                   header=TRUE,                    
                   stringsAsFactors = FALSE)     

attach(Testing6)

summary(Testing6)

str(Testing6)

View(Testing6)

Model_6<-aov(biomass~treatment*time, data=Testing6)

summary(Model_6)

##Assumption testig:##

par(mfrow=c(2,2))

plot(Model_6)

#####Shapiro-Wilk test (testing for normality)####

uhat6<-resid(Model_6)
shapiro.test(uhat6)

####Assumption 2: Homogeneity of variance of the groups###

leveneTest(biomass~treatment*time, data=Testing6)

