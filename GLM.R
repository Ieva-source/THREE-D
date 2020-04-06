###General linear model for particular plant species###

GLM<-read.csv("GLM.csv",  
             sep=";",                        
             dec=",",                        
             header=TRUE,                    
             stringsAsFactors = FALSE)

# Logistic Regression
# where F is a binary factor and
# x1-x3 are continuous predictors
fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())

Agr <- glm(Agrostis.capillaris~Site, data=GLM, family = binomial())
summary(Agr) # display results

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

confint(Agr) # 95% CI for the coefficients
exp(coef(Agr)) # exponentiated coefficients
exp(confint(Agr)) # 95% CI for exponentiated coefficients
predict(Agr, type="response") # predicted values
residuals(Agr, type="deviance") # residuals

Anth <- glm(Anthoxanthum.odoratum~Site, data=GLM, family=binomial())
summary(Anth)

Ave <- glm(Avenella.flexuosa~Site, data=GLM, family=binomial())
summary(Ave)

Des <- glm(Deschampsia.cespitosa~Site, data=GLM, family=binomial())
summary(Des)

Poa <- glm(Poa.pratensis~Site, data=GLM, family=binomial())
summary(Poa)

Rum <- glm(Rumex.acetosa~Site, data=GLM, family=binomial())
summary(Rum)

Carex <- glm(Carex.sp.~Site, data=GLM, family=binomial())
summary(Carex)

Big <- glm(Carex.bigellowi~Site, data=GLM, family=binomial())

summary(Big)


##GLM: Family=poission##

Agr <- glm(Agrostis.capillaris~Site, data=GLM, family = poisson())
summary(Agr) # display results

anova(Agr, test="Chisq")

confint(Agr) # 95% CI for the coefficients
exp(coef(Agr)) # exponentiated coefficients
exp(confint(Agr)) # 95% CI for exponentiated coefficients
predict(Agr, type="response") # predicted values
residuals(Agr, type="deviance") # residuals

Anth <- glm(Anthoxanthum.odoratum~Site, data=GLM, family=poisson())
summary(Anth)

Ave <- glm(Avenella.flexuosa~Site, data=GLM, family=poisson())
summary(Ave)

Des <- glm(Deschampsia.cespitosa~Site, data=GLM, family=poisson())
summary(Des)

Poa <- glm(Poa.pratensis~Site, data=GLM, family=poisson())
summary(Poa)

Rum <- glm(Rumex.acetosa~Site, data=GLM, family=poisson())
summary(Rum)

Carex <- glm(Carex.sp.~Site, data=GLM, family=poisson())
summary(Carex)

Big <- glm(Carex.bigellowi~Site, data=GLM, family=poisson())
summary(Big)