library(readxl)
COMBINED <- read_excel("Downloads/ECO 205 Final Paper Data.xls")
View(COMBINED)

install.packages("moments")
library(moments)
install.packages("stargazer")
library(stargazer)
library(dplyr)
library(readxl) 
library(sandwich) 
library(lmtest)
library(sjPlot) 
library(Greg) 
library(car) 
library(ggplot2)

Project2=data.frame(UnemploymentRate = COMBINED$"Growth Rate in Unemployment Rate, Seasonaly Adjusted from January 31 to July 31 2021",
                    PriceLevel = COMBINED$"Growth Rate in Increase in Prices Rate from January 31 to July 31 2021",
                    HomeValue = COMBINED$"Growth Rate in United States Median House Prices in terms of Home Value in USD from January 31 to July 31 2021",
                    GDPPC = COMBINED$"Growth Rate in Current-Dollar GDP in millions of USD from January 31 to July 31 2021",
                    Vacancy = COMBINED$"Growth Rate in Homeowner Vacancy Rates from January 31 to July 31 2021",
                    PopChange = COMBINED$"Growth Rate in United States Population from January 31 to July 31 2021") 
View(Project2)
Averages=sapply(Project2, mean) 
print(Averages)
Stdevs=sapply(Project2, sd)
Mins=sapply(Project2, min)
Q1s=sapply(Project2, quantile, 0.25)
Medians=sapply(Project2, median)
Q3s=sapply(Project2, quantile, 0.75)
Maxs=sapply(Project2, max)
Numobs=nrow(Project2)-sapply(Project2, function(x) sum(is.na(x))) 
Skewvals=sapply(Project2, skewness)
Kurtvals=sapply(Project2, kurtosis)

Summary=data.frame(Average = Averages, 
                   Std_Dev = Stdevs, 
                   Minimum = Mins,
                   First_Quartile = Q1s,
                   Median = Medians,
                   Third_Quartile = Q3s,
                   Maximum = Maxs,
                   Skewnewss = Skewvals,
                   Kurtosis = Kurtvals,
                   Number_of_Obs = Numobs)

print(Summary) 

DistributionSummaryOrganized <- t(Summary) 
print(DistributionSummaryOrganized) 

stargazer(DistributionSummaryOrganized, summary=FALSE,type='text')

#model 1 computation
model1 = lm(HomeValue ~  PriceLevel + UnemploymentRate + GDPPC + PopChange + Vacancy, data=Project2)
summary(model1)
AIC1=AIC(model1)
AIC1

#model 2 computation
Project2$HomeValueLn=log(Project2$HomeValue)
Project2$VacancyLn=log(Project2$Vacancy)
Project2$VacancyLnPop = Project2$VacancyLn * Project2$PopChange
model2 = lm(HomeValueLn ~  PriceLevel + UnemploymentRate + GDPPC + PopChange + VacancyLn + VacancyLnPop, data=Project2)
summary(model2)
AIC2=AIC(model2)
AIC2

#model 3 computation
Project2$UnemploymentRate2=Project2$UnemploymentRate^2
Project2$VacancyLnPriceLevel = Project2$VacancyLn * Project2$PriceLevel
model3e = lm(HomeValueLn ~  PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data=Project2)
summary(model3)
AIC3=AIC(model3)
AIC3

#Correcting for Heteroskedasticity
vcov1 <- vcovHC(model1, type = "HC1")
vcov1
robust_se1 <- sqrt(diag(vcov1))
robust_se1
coeftable_robust1=coeftest(model1, vcov.=vcov1)
coeftable_robust1
tab_model(model1,vcov.fun = vcov1, show.se = TRUE)

vcov2 <- vcovHC(model2, type = "HC1")
vcov2
robust_se2 <- sqrt(diag(vcov2))
robust_se2
coeftable_robust2=coeftest(model2, vcov.=vcov)
coeftable_robust2
tab_model(model2,vcov.fun = vcov2, show.se = TRUE)

vcov3 <- vcovHC(model3, type = "HC1")
vcov3
robust_se3 <- sqrt(diag(vcov3))
robust_se3
coeftable_robust3=coeftest(model3, vcov.=vcov3)
coeftable_robust3
tab_model(model3,vcov.fun = vcov3, show.se = TRUE)

#Ramsay RESET Test
resettest(model1,power=2:3)
resettest(model2,power=2:3)
resettest(model3,power=2:3)

#VIF calculation
vif(model1)
vif(model2)
vif(model3)

#Computing Outliers for Model 3
Project2Outliers=Project2
View(Project2Outliers)


#1. Leverage: 
Project2Outliers$hats=hatvalues(model3)
View(Project2Outliers)

#2. Studentized residuals: 
Project2Outliers$rstuds=rstudent(model3)
View(Project2Outliers)
summary(model3)
tcrit=qt(p=.05/2, model3$df.residual-1, lower.tail=FALSE)
tcrit
Project2Outliers$rstudoutliers=ifelse(abs(Project2Outliers$rstuds)>tcrit,1,0) 

Project2.without.Rstud.outliers = subset(Project2Outliers, rstudoutliers!=1) 
View(Project2.without.Rstud.outliers)
model3robust1=lm(HomeValueLn ~  PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data=Project2.without.Rstud.outliers)
summary(model3robust1)
summary(model3) 
AIC4=AIC(model3robust1)
AIC4
vif(model3robust1)
resettest(model3robust1,power=2:3)

#3. DFFITS: 
Project2Outliers$dffitsvals=dffits(model3)
p = length(model3$coefficients)-1
n = nrow(Project2)
DFFITSthresh <- 2*sqrt(p/n)
DFFITSthresh
Project2Outliers$dffitsoutliers=ifelse(abs(Project2Outliers$dffitsvals)>DFFITSthresh,1,0) 
Project2.without.DFFITS.outliers = subset(Project2Outliers, dffitsoutliers!=1) 
View(Project2.without.DFFITS.outliers)
model3robust2=lm(HomeValueLn ~  PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data=Project2.without.DFFITS.outliers)
summary(model3robust2)
summary(model3) 

#Subset F tests for Price Level
linearHypothesis(model3, c("PriceLevel", "VacancyLnPriceLevel"))
linearHypothesis(model3, c("PriceLevel", "VacancyLnPriceLevel"), white.adjust = "hc1") #Corrected for heteroskedasticity 

#Subset F tests for Vacancy
linearHypothesis(model3, c("VacancyLn","VacancyLnPop", "VacancyLnPriceLevel"))
linearHypothesis(model3, c("VacancyLn","VacancyLnPop", "VacancyLnPriceLevel"), white.adjust = "hc1") #Corrected for heteroskedasticity

#Plotting Marginal Influence of Price Level 
regvcov=vcov(model3)
regvcov

#plot the confidence interval for dLnHomeValue/dPriceLevel across values of dPriceLevel
curve(coef(summary(model3))[2,1]+x*coef(summary(model3))[9,1], from=DistributionSummaryOrganized[3,2],
      to=DistributionSummaryOrganized[7,2], ylim=c(-0.05, 0.05), 
      col='blue', xlab="Price Level (Gross Growth Rate)",
      ylab="dHomeValueLn/dPriceLevel") 
curve(coef(summary(model3))[2,1]*DistributionSummaryOrganized[1,7]+x*coef(summary(model3))[9,1]*DistributionSummaryOrganized[1,7]+1.96*sqrt(regvcov[2,2] + x^2*regvcov[9,9] + 2*x*regvcov[2,9]), from=DistributionSummaryOrganized[3,2], to=DistributionSummaryOrganized[7,2], add=TRUE, ylim=c(-0.05, 0.05), col='red')
curve(coef(summary(model3))[2,1]*DistributionSummaryOrganized[1,7]+x*coef(summary(model3))[9,1]*DistributionSummaryOrganized[1,7]-1.96*sqrt(regvcov[2,2] + x^2*regvcov[9,9] + 2*x*regvcov[2,9]), from=DistributionSummaryOrganized[3,2], to=DistributionSummaryOrganized[7,2], add=TRUE, ylim=c(-0.05, 0.05), col='red')

#can repeat with vcovHC
curve(100*(coef(summary(model3))[2,1]+x*coef(summary(model3))[9,1]), from=DistributionSummaryOrganized[3,8],
      to=DistributionSummaryOrganized[7,8], ylim=c(-12,12),
      col='blue', xlab="Gross Growth Rate of Vacancy Levels (Ln)",
      ylab="(dHomeValueLn/HomeValueLn*100%)/dPriceLevel") 
curve(100*(coef(summary(model3))[2,1]+x*coef(summary(model3))[9,1]+1.96*sqrt(hetvcov[2,2] + x^2*hetvcov[9,9] + 2*x*hetvcov[2,9])), from=DistributionSummaryOrganized[3,8], to=DistributionSummaryOrganized[7,8], add=TRUE, col='red')
curve(100*(coef(summary(model3))[2,1]+x*coef(summary(model3))[9,1]-1.96*sqrt(hetvcov[2,2] + x^2*hetvcov[9,9] + 2*x*hetvcov[2,9])), from=DistributionSummaryOrganized[3,8], to=DistributionSummaryOrganized[7,8], add=TRUE, col='red')

#Scatter plot for PriceLevel values
plot(Project2$PriceLevel, Project2$HomeValueLn, xlab="Gross Growth Rate of Price Level", ylab="Gross Growth Rate of Home Value (Ln)", xlim=c(0, 2.5), ylim=c(0,0.25)) 
abline(v=DistributionSummaryOrganized[5,2], col="blue", lwd=3)
text(x=0.75, y=0.2, 'Median ==>')

Project2SubLow=subset(Project2, PriceLevel<DistributionSummaryOrganized[5,2])
#View(Project2SubLow)
low = lm(HomeValueLn ~  PriceLevel, data=Project2SubLow)
summary(low)
#abline(low)
segments(x0=0,y0=low$coefficients[1],x1=DistributionSummaryOrganized[5,2],y1=low$coefficients*DistributionSummaryOrganized[5,2],col="red", lwd=2)

Project2SubHigh=subset(Project2, PriceLevel>DistributionSummaryOrganized[5,2])
#View(Project2SubLow)
high = lm(HomeValueLn ~  PriceLevel, data=Project2SubHigh)
summary(high)
#abline(high)
segments(x0=DistributionSummaryOrganized[5,2],y0=high$coefficients[1]+high$coefficients[2]*DistributionSummaryOrganized[5,2],x1=2.5,y1=high$coefficients[1]+high$coefficients[2]*2.5,col="green",lwd=2)

#Endogeneity Check
#Perform the first stage
Project2$rankPriceLevel=rank(Project2$PriceLevel)
model3_multi_s1 <- lm(PriceLevel ~ UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel + rankPriceLevel, data = Project2)
summary(model3_multi_s1)
coeftest(model3_multi_s1, vcov = vcovHC, type = "HC1")

#Do IVreg
model3_multi <- ivreg(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel | UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel + rankPriceLevel, data = Project2)
coeftest(model3_multi, vcov = vcovHC, type = "HC1")

#Do Hausman Test
Project2$multi_pred <- model3_multi_s1$fitted.values
Project2$multi_resid <- model3_multi_s1$residuals

model3_multi_hausman1 <- lm(HomeValueLn ~  PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel + multi_pred, data=Project2)
coeftest(model3_multi_hausman1, vcov = vcovHC, type = "HC1")

model3_multi_hausman2 <- lm(HomeValueLn ~  PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel + multi_resid, data=Project2)
coeftest(model3_multi_hausman2, vcov = vcovHC, type = "HC1")

#Utilizing PanelData Techniques
Project2Panel=data.frame(UnemploymentRate = COMBINED$"Growth Rate in Unemployment Rate, Seasonaly Adjusted from January 31 to July 31 2021",
                    PriceLevel = COMBINED$"Growth Rate in Increase in Prices Rate from January 31 to July 31 2021",
                    HomeValue = COMBINED$"Growth Rate in United States Median House Prices in terms of Home Value in USD from January 31 to July 31 2021",
                    GDPPC = COMBINED$"Growth Rate in Current-Dollar GDP in millions of USD from January 31 to July 31 2021",
                    Vacancy = COMBINED$"Growth Rate in Homeowner Vacancy Rates from January 31 to July 31 2021",
                    PopChange = COMBINED$"Growth Rate in United States Population from January 31 to July 31 2021",
                    State=COMBINED$"State",
                    Period=COMBINED$"Period") 

Project2Panel$State=as.factor(Project2Panel$State) #this function converts any variable to a categorical variables - it considers each unique entry in the column as its own category
str(Project2Panel)

#defining variables
Project2Panel$HomeValueLn=log(Project2Panel$HomeValue)
Project2Panel$VacancyLn=log(Project2Panel$Vacancy)
Project2Panel$VacancyLnPop = Project2Panel$VacancyLn * Project2Panel$PopChange
Project2Panel$UnemploymentRate2=Project2Panel$UnemploymentRate^2
Project2Panel$VacancyLnPriceLevel = Project2Panel$VacancyLn * Project2Panel$PriceLevel

#model 3 computations
panel_pind3 <- plm(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data = Project2Panel, index=c("State", "Period"), model="pooling")
summary(panel_pind3)
panel_stateind3 <- plm(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data = Project2Panel, index=c("State", "Period"), model="within", effect="individual")
summary(panel_stateind3)
panel_timeind3 <- plm(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data = Project2Panel, index=c("State", "Period"), model="within", effect="time")
summary(panel_timeind3)
panel_twowaysind3 <- plm(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel, data = Project2Panel, index=c("State", "Period"), model="within", effect="twoways")
summary(panel_twowaysind3)

#reporting standard errors
rob_seind <- list(sqrt(diag(vcovHC(panel_pind3, type = "HC1"))),
                  sqrt(diag(vcovHC(panel_stateind3, type = "HC1"))),
                  sqrt(diag(vcovHC(panel_timeind3, type = "HC1"))),
                  sqrt(diag(vcovHC(panel_twowaysind3, type = "HC1"))))


stargazer(panel_pind3, panel_stateind3, panel_timeind3, panel_twowaysind3, 
          type="text",
          header= FALSE,
          se=rob_seind,
          title='Panel models with home value')

#F-test to determine best effects
pFtest(panel_stateind3, panel_pind3) 
pFtest(panel_timeind3, panel_pind3) 
pFtest(panel_twowaysind3, panel_stateind3) 
pFtest(panel_twowaysind3, panel_timeind3) 
pFtest(panel_twowaysind3, panel_pind3) 

#OLS with factor (state and period)
model3olsplm <- lm(HomeValueLn ~ PriceLevel + UnemploymentRate + UnemploymentRate2 + GDPPC + PopChange + VacancyLn + VacancyLnPop + VacancyLnPriceLevel + factor(State) + factor(Period), data = Project2Panel)
summary(model3olsplm)
AIC3plm=AIC(panel_twowaysind3)
AIC3plm
vif(model3olsplm)

#Ramsay RESET (w/ factor model) and Subset F-Tests on Panel model
resettest(model3olsplm)
linearHypothesis(panel_twowaysind3, c("PriceLevel", "VacancyLnPriceLevel"), white.adjust = "hc1") #Corrected for heteroskedasticity 
linearHypothesis(panel_twowaysind3, c("VacancyLn","VacancyLnPop", "VacancyLnPriceLevel"), white.adjust = "hc1") #Corrected for heteroskedasticity
