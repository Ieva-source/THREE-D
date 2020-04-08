###General linear model for particular plant species###

GLM<-read.csv("GLM.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)

library(aod)
library("lsmeans")
library("multcompView")
library("car")

# Logistic Regression
# where F is a binary factor and
# x1-x3 are continuous predictors (example)

Agr <- glm(Agrostis.capillaris~Site, data=GLM, family = binomial())#GLM
summary(Agr) # display results
confint(Agr) # 95% CI for the coefficients
exp(coef(Agr)) # exponentiated coefficients
exp(confint(Agr)) # 95% CI for exponentiated coefficients
predict(Agr, type="response") # predicted values
residuals(Agr, type="deviance") # residuals
wald.test(b = coef(Agr), Sigma = vcov(Agr), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Agr,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)


###Anthoxanthum###

Anth <- glm(Anthoxanthum.odoratum~Site, data=GLM, family=binomial())
summary(Anth)
confint(Anth) # 95% CI for the coefficients
exp(coef(Anth)) # exponentiated coefficients
exp(confint(Anth)) # 95% CI for exponentiated coefficients
predict(Anth, type="response") # predicted values
residuals(Anth, type="deviance") # residuals
wald.test(b = coef(Anth), Sigma = vcov(Anth), Terms = 2:4)# Wald test for the effect of root
Anova(Anth, type=3)
posthoc <-lsmeans(Anth,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Avenella###
Ave <- glm(Avenella.flexuosa~Site, data=GLM, family=binomial())
summary(Ave)
confint(Ave) # 95% CI for the coefficients
exp(coef(Ave)) # exponentiated coefficients
exp(confint(Ave)) # 95% CI for exponentiated coefficients
predict(Ave, type="response") # predicted values
residuals(Ave, type="deviance") # residuals
wald.test(b = coef(Ave), Sigma = vcov(Ave), Terms = 2:2)# Wald test for the effect of root
Anova(Ave, type=3)
posthoc <-lsmeans(Ave,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Deschampsia###

Des <- glm(Deschampsia.cespitosa~Site, data=GLM, family=binomial())
summary(Des)
confint(Des) # 95% CI for the coefficients
exp(coef(Des)) # exponentiated coefficients
exp(confint(Des)) # 95% CI for exponentiated coefficients
predict(Des, type="response") # predicted values
residuals(Des, type="deviance") # residuals
wald.test(b = coef(Des), Sigma = vcov(Des), Terms = 2:4)# Wald test for the effect of root
Anova(Des, type=3)
posthoc <-lsmeans(Des,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Poa###
Poa <- glm(Poa.pratensis~Site, data=GLM, family=binomial())
summary(Poa)
confint(Poa) # 95% CI for the coefficients
exp(coef(Poa)) # exponentiated coefficients
exp(confint(Poa)) # 95% CI for exponentiated coefficients
predict(Poa, type="response") # predicted values
residuals(Poa, type="deviance") # residuals
wald.test(b = coef(Poa), Sigma = vcov(Poa), Terms = 2:3)# Wald test for the effect of root
Anova(Poa, type=3)
posthoc <-lsmeans(Poa,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Rumex###
Rum <- glm(Rumex.acetosa~Site, data=GLM, family=binomial())
summary(Rum)
confint(Rum) # 95% CI for the coefficients
exp(coef(Rum)) # exponentiated coefficients
exp(confint(Rum)) # 95% CI for exponentiated coefficients
predict(Rum, type="response") # predicted values
residuals(Rum, type="deviance") # residuals
wald.test(b = coef(Rum), Sigma = vcov(Rum), Terms = 2:2)# Wald test for the effect of root
Anova(Rum, type=3)
posthoc <-lsmeans(Rum,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Carex###
Carex <- glm(Carex.sp.~Site, data=GLM, family=binomial())
summary(Carex)
confint(Carex) # 95% CI for the coefficients
exp(coef(Carex)) # exponentiated coefficients
exp(confint(Carex)) # 95% CI for exponentiated coefficients
predict(Carex, type="response") # predicted values
residuals(Carex, type="deviance") # residuals
wald.test(b = coef(Carex), Sigma = vcov(Carex), Terms = 2:3)# Wald test for the effect of root
Anova(Carex, type=3)
posthoc <-lsmeans(Carex,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

###Bigelowi###
Big <- glm(Carex.bigellowi~Site, data=GLM, family=binomial())

summary(Big)
confint(Big) # 95% CI for the coefficients
exp(coef(Big)) # exponentiated coefficients
exp(confint(Big)) # 95% CI for exponentiated coefficients
predict(Big, type="response") # predicted values
residuals(Big, type="deviance") # residuals
wald.test(b = coef(Big), Sigma = vcov(Big), Terms = 2:4)# Wald test for the effect of root
Anova(Big, type=3)
posthoc <-lsmeans(Big,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

summary(posthoc)
plot(posthoc)

########################
##GLM: Family=poission##

GLM2<-read.csv("GLM_poisson.csv",  
              sep=";",                        
              dec=",",                        
              header=TRUE,                    
              stringsAsFactors = FALSE)


Agr <- glm(Agrostis.capillaris~Site, data=GLM2, family = poisson())
summary(Agr) # display results
wald.test(b = coef(Agr), Sigma = vcov(Agr), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Agr,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Anth <- glm(Anthoxanthum.odoratum~Site, data=GLM2, family=poisson())
summary(Anth)
wald.test(b = coef(Anth), Sigma = vcov(Anth), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Anth,
                  pairwise~Site,
                  adjust = "tukey")

Ave <- glm(Avenella.flexuosa~Site, data=GLM2, family=poisson())
summary(Ave)
wald.test(b = coef(Ave), Sigma = vcov(Ave), Terms = 2:2)# Wald test for the effect of root
posthoc <-lsmeans(Ave,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Des <- glm(Deschampsia.cespitosa~Site, data=GLM2, family=poisson())
summary(Des)
wald.test(b = coef(Des), Sigma = vcov(Des), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Des,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Poa <- glm(Poa.pratensis~Site, data=GLM2, family=poisson())
summary(Poa)
wald.test(b = coef(Poa), Sigma = vcov(Poa), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Poa,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Rum <- glm(Rumex.acetosa~Site, data=GLM2, family=poisson())
summary(Rum)
wald.test(b = coef(Rum), Sigma = vcov(Rum), Terms = 2:2)# Wald test for the effect of root
posthoc <-lsmeans(Rum,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Carex <- glm(Carex.sp.~Site, data=GLM2, family=poisson())
summary(Carex)
wald.test(b = coef(Carex), Sigma = vcov(Carex), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Carex,
                  pairwise~Site,
                  adjust = "tukey")
posthoc

Big <- glm(Carex.bigellowi~Site, data=GLM2, family=poisson())
summary(Big)
wald.test(b = coef(Big), Sigma = vcov(Big), Terms = 2:3)# Wald test for the effect of root
posthoc <-lsmeans(Big,
                  pairwise~Site,
                  adjust = "tukey")
posthoc
##How well our model fits depends on the difference between the model and the observed data.  
#One approach for binary data is to implement a Hosmer Lemeshow goodness of fit test.

install.packages("ResourceSelection")
library(ResourceSelection)

hl<- hoslem.test(GLM$Agrostis.capillaris, fitted(Agr))
hl
##Hosmer and Lemeshow goodness of fit (GOF) test (used for Hosmer-Lemeshow goodness of fit test is overall used to assess goodness of fit in logistic regression with individual binary data.)
##p value was NA
cbind(hl$observed,hl$expected)


anova(Agr, test="Chisq")
exp(2.8622)
(17.49998-1)*100
plot(Site, fitted.values(logAgr))
