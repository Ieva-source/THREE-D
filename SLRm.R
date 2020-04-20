#####Simple linear regresion model#####

SLR<-read.csv("SLR.csv",  
              sep=";",                        
              dec=",",                        
              header=TRUE,                    
              stringsAsFactors = FALSE)

library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

#####Scatter plot to check for linearity and additivness assumption
scatter.smooth(x=SLR$Elevation, y=SLR$Biomass, main="Biomass ~ Elevation")

#####Checking for outliers#####

par(mfrow=c(1, 1))  # divide graph area in 2 columns
boxplot(SLR$Elevation, main="Elevation", sub=paste("Outlier rows: ", boxplot.stats(SLR$Elevation)$out))  # box plot for 'elev'
boxplot(SLR$Biomass, main="Biomass", sub=paste("Outlier rows: ", boxplot.stats(SLR$Biomass)$out))  # box plot for 'biomass'

#####Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(SLR$Elevation), main="Density Plot: Elevation", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(SLR$Elevation), 2)))  # density plot for 'speed'
polygon(density(SLR$Elevation), col="red")
plot(density(SLR$Biomass), main="Density Plot: Biomass", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(SLR$Biomass), 2)))  # density plot for 'biomass'
polygon(density(SLR$Biomass), col="red")

cor(SLR$Elevation, SLR$Biomass)  # calculate correlation between elevation and biomass 
#####Conclusion: Strong inverse relationship between elevation and biomass (-0.797)

# building linear regression model
linearMod <- lm(Elevation ~ Biomass, data=SLR)
print(linearMod)
summary(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Biomass", "Estimate"]  # get beta estimate for biomass
std.error <- modelCoeffs["Biomass", "Std. Error"]  # get std.error for biomass
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(linearMod)  
BIC(linearMod)

ggplot(SLR, aes(x = Elevation, y = Biomass)) +
  geom_point() +
  stat_smooth()
cor(SLR$Elevation, SLR$Biomass)

model15 <- lm(Elevation ~ Biomass, data = SLR)
model15

SLR<- ggplot(SLR, aes(Elevation, Biomass)) +
  geom_point() +
  stat_smooth(method = lm)+
  labs(x = "Elevation m a.s.l.", y = "Grazing effect (g)") +
  scale_fill_brewer(palette = "Accent") + theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma", face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "bottom")
SLR


summary(model15)
confint(model15)
sigma(model15)*100/mean(SLR$Elevation)
