library(stargazer)
library(corrplot)
library(corrr)
library(tidyverse)
library(tidyr)
library(pastecs)
library(purrr)
library(psych)
library(car)
library(quantmod)
library(forecast)
library(xlsx)
library(rJava)
library(fpp2)
library(reshape2)
library(gridExtra)

#starting with supply variables

canm1 <- lm(REARN ~ PL + LBC + UC + UED + UINF + LBCN + LIC, data = CompsData)
summary(canm1)
canm1df <- data.frame(PL, LBC, UC, UED, UINF, LBCN, LIC)

canm1_1 <- lm(REARN ~ PL + LBC + UED + UINF + LBCN + LIC + TIME, data = CompsData)
summary(canm1_1)

canm2 <- lm(REARN ~ PL + LBC + UC + UED + UINF + LBCN, data = CompsData)
summary(canm2)

canm3 <- lm(REARN ~ PL + LBC + UED + UINF + LBCN, data = CompsData)
summary(canm3)

lnPL <- log(CompsData$PL)

canm4 <- lm(REARN ~ lnPL + LBC + UED +UINF +LBCN, data = CompsData)
summary(canm4)

lnPL
lnREARN <- log(CompsData$REARN)

#best supply
canm5 <- lm(lnREARN ~ PL + LBC + UED +UINF + LBCN, data = CompsData)
summary(canm5)

#combining

canm6 <- lm(REARN ~ R + USPCE + INC + U + PHO + TEMP + 
              FLT + CRI + COT + UST, data = CompsData)
summary(canm6)
canm6df <- data.frame(R, USPCE, INC, U, PHO, TEMP,
                      FLT, CRI, COT, UST)
corcanm6 <- cor(canm6df, use = "pairwise.complete.obs")

alias(canm6)
#low observations issue

canm6_1 <- lm(REARN ~ R + DPI + USPCE + U + LFP + PHO + TEMP + 
                FLT + CRI + COV + COT + UST, data = CompsData)

canm7 <- lm(REARN ~ USPCE + INC + U + PHO + TEMP + 
              FLT + CRI + COT + UST, data = CompsData)
summary (canm7)

canm8 <- lm(REARN ~ USPCE + INC + U + PHO + TEMP + FLT + CRI + COT, data = CompsData)
summary(canm8)

canm9 <- lm(REARN ~ USPCE + INC + U + TEMP + FLT + CRI + COT, data = CompsData)
summary(canm9)

canm10 <- lm(REARN ~ USPCE + U + TEMP + FLT + CRI + COT, data = CompsData)
summary(canm10)

canm11 <- lm(REARN ~ USPCE + U + FLT + CRI + COT, data = CompsData)
summary(canm11)

canm12 <- lm(REARN ~ USPCE + U + FLT + CRI, data = CompsData)
summary(canm12)

#best of first 2 groups
canm13 <- lm(lnREARN ~ USPCE + U + FLT + CRI, data = CompsData)
summary(canm13)

#combining all
canm14 <- lm(lnREARN ~ USPCE + U + FLT + CRI + PL + LBC + UED +
               UINF + LBCN, data = CompsData)
summary(canm14)

canm15 <- lm(lnREARN ~ USPCE + U + FLT + PL + LBC + UED + UINF + LBCN, data = CompsData)
summary(canm15)

#best with all - observations low
canm16 <- lm(lnREARN ~ USPCE + U + CRI + PL + LBC + UED +UINF + LBCN, data = CompsData)
summary(canm16)

canm17 <- lm(lnREARN ~ R + USPCE + INC + U + PHO + TEMP + 
               FLT + CRI + COV + SIP + COT + UST + PL + LBC + UC + UED + 
               UINF + LBCN + LIC, data = CompsData)
alias(canm17)
summary(canm17)

canm18 <- lm(lnREARN ~ USPCE + U + CRI + PL + LBC + 
               UED +UINF + LBCN + LIC, data = CompsData)
summary(canm18)

canm19 <- lm(lnREARN ~ USPCE + U + CRI + PL + LBC + 
               UED + LBCN + LIC, data = CompsData)
summary(canm19)

canm20 <- lm(lnREARN ~ USPCE + U + PL + LBC + UED + 
               UINF + LBCN + LIC, data = CompsData)
summary(canm20)

canm21 <- lm(lnREARN ~ USPCE + U + PL + CRI + LBC + UED + 
               UINF + LBCN + LIC + INC, data = CompsData)
summary(canm21)

canm22 <- lm(lnREARN ~ USPCE + U + CRI + PL + LBC + 
               UED + UINF + LBCN, data = CompsData)

summary(canm22)

canm23 <- lm(lnREARN ~ USPCE + U + CRI + LIC, data = CompsData)
summary(canm23)

#Using the natural log instead
lnUSPCE <- log(CompsData$USPCE)

canm24 <- lm(lnREARN ~ lnUSPCE + U + CRI + LIC + FLT, data = CompsData)
summary(canm24)

#best but observations far too low
canm25 <- lm(lnREARN ~ lnUSPCE + U + CRI + LIC + PL + UED, data = CompsData)
summary(canm25)

#getting observations up - best first regression
canm26 <- lm(lnREARN ~ lnUSPCE + U + LIC + PL + UED, data = CompsData)
summary(canm26)

#correlation
attach(CompsData)
canm26df <- data.frame(USPCE, U, LIC, PL, UED)
corcanm26 <- cor(canm26df, use = "pairwise.complete.obs")
corcanm26
corrplot(corcanm26, method = 'color', addCoef.col = "grey")

#Exporting the results into table format
#table 1
stargazer(list(canm1,canm5,canm6,canm13,canm14, canm26), 
          title = "CompsTable1", type = "html", out = "firstcomps3.doc", 
          report = "vct*", star.cutoffs = c(.05), order=c("DPI", "Isq"), 
          keep.stat = c("rsq", "adj.rsq", "f", "wald", "n", "ser") )

#residual plots
library(ggplot2)
ggplot(canm26)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm5)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm13)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm13)+geom_point(aes(x=.fitted, y=.resid))

#best correlation matrix - can't get compsdata to work
cor(canm26st, use = "pairwise.complete.obs")
cor(as.numeric(CompsData), use = "pairwise.complete.obs")

#introducing time and COV which seems to work now
canm27 <- lm(lnEARN ~ lnUSPCE + U + LIC + UED + TIME + TIMEsq + COV, data = CompsData)
summary(canm27)

#back to analyzing log vs regular
canm28 <- lm(REARN ~ USPCE + U + FLT + COV, data = CompsData)
summary(canm28)
ggplot(canm28)+geom_point(aes(x=.fitted, y=.resid))

#introduce new variables, work on getting n = 82
#put back lnPL and TIME, no UED
canm29 <- lm(REARN ~ USPCE + U + FLT + COV + lnPL + TIME + TIMEsq, data = CompsData)
summary(canm29)
canm29st <- data.frame(CompsData$USPCE, CompsData$U, CompsData$FLT, CompsData$COV, 
                       CompsData$TIME, CompsData$PL)
canm29st
cor(canm29st, use = "pairwise.complete.obs")

#introduce DPI, a new variable and replace USPCE with it
lnDPI <- log(DPI)
canm30 <- lm(REARN ~ lnDPI + U + lnFLT + COV + lnPL + TIME + 
               TIMEsq + COT, data = CompsData)
summary(canm30)
#DPI is more significant than USPCE, but the sign is unexpected
#could indicate that when people have more money to spend they purchase less weed
#more means to do other things like go out or travel
canm30df <- data.frame(lnDPI, CompsData$U, lnFLT, lnPL, CompsData$COV, 
                       CompsData$TIME, CompsData$COT)
canm30df
cor(canm30df, use = "pairwise.complete.obs")
lnFLT <- log(CompsData$FLT)
canm31 <- lm(lnREARN ~ DPI + U + lnFLT + COV + lnPL + TIME + TIMEsq, data = CompsData)
summary(canm31)
ggplot(canm30)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm31)+geom_point(aes(x=.fitted, y=.resid))

pairs(canm30df)
ggplot(CompsData, aes(x=U, y=REARN)) + geom_point()
ggplot(CompsData, aes(x=DPI, y=REARN)) + geom_point()
ggplot(CompsData, aes(x=PL, y=REARN)) + geom_point()

#analyzing scatter plots to reduce multicoliniarity
Usq <- (CompsData$U)^2
canm32 <- lm(REARN ~  lnDPI + U + Usq + lnFLT + COV + lnPL + 
               TIME + TIMEsq + COT, data = CompsData)
summary(canm32)
canm32df <- data.frame(lnDPI, U, Usq, lnFLT, COV, lnPL, TIME, TIMEsq, COT)
corcanm32 <- cor(canm32df, use = "pairwise.complete.obs")
corrplot(as.matrix(corcanm32), method = 'color', addCoef.col = "grey")

#introduce labor participation
canm33 <- lm(REARN ~ lnDPI + LFP + lnFLT + COV + lnPL + 
               TIME + TIMEsq + COT, data = CompsData)
summary(canm33)
canm33df <- data.frame(lnDPI, LFP, lnFLT, COV, lnPL, TIME, TIMEsq, COT)
cor(canm33df, use = "pairwise.complete.obs")
ggplot(CompsData, aes(x=LFP, y=REARN)) + geom_point()

ggplot(canm33)+geom_point(aes(x=.fitted, y=.resid))

#introduce month dummy
canm34 <- lm(REARN ~ lnDPI + LFP + lnFLT + COV + lnPL + 
               COT + JAN + FEB + MAR + APR + 
               MAY + JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm34)
attach(CompsData)
canm34df <- data.frame(lnDPI, LFP, lnFLT, COV, lnPL, 
                       COT, JAN, FEB, MAR, APR, 
                       MAY, JUN, JUL, AUG, SEP, OCT, NOV)
corcanm34 <- cor(canm34df, use = "pairwise.complete.obs")

#visualizing correlation using the corrplot package
corcanm33 <- cor(canm33df, use = "pairwise.complete.obs")
corcanm26 <- cor(canm26df, use = "pairwise.complete.obs")

corrplot(as.matrix(corcanm26), method = 'color', addCoef.col = "grey")

palette <- colorRampPalette(c("green", "white", "red")) (20) 
heatmap(x = corcanm33, col = palette, symm = TRUE)

corrplot(as.matrix(corcanm34), method = 'color', addCoefasPercent = TRUE, 
         addCoef.col = "grey")

pairs(canm33df, lower.panel = NULL)

# Printing scatterplots within for-loop
for(i in 2:ncol(canm33df)) {                              
  print(ggplot(canm33df, aes(x = canm33df [ , i], y = CompsData$REARN)) +
          geom_point())
  Sys.sleep(2)
}

ggplot(CompsData, aes(x= DPI, y=REARN)) + geom_point()

DPIsq <- DPI^2

#Finding other ways to reduce underspecification
canm35 <- lm(REARN ~ DPI + DPIsq + LFP + lnFLT + COV + lnPL + COT +
               JAN + FEB + MAR + APR + MAY + 
               JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm35)
canm35df <- data.frame(DPI, DPIsq, LFP, lnFLT, COV, lnPL, COT,
                       JAN, FEB, MAR, APR, MAY, 
                       JUN, JUL, AUG, SEP, OCT, NOV)
corcanm35 <- cor(canm35df, use = "pairwise.complete.obs")
corrplot(as.matrix(corcanm35), method = 'color', addCoef.col = "grey")

#More messing around - ln / sq
lnLIC <- log(CompsData$LIC)
canm36 <- lm(lnREARN ~ DPI + DPIsq + LFP + lnFLT + COV + lnPL + COT +
               JAN + FEB + MAR + APR + MAY + 
               JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm36)
# LIC is highly correlated with PL, only one measure of supply is necesarry
ggplot(canm35)+geom_point(aes(x=.fitted, y=.resid))

#table 2
stargazer(list(canm5,canm18, canm26, canm33,canm35, canm36), 
          title = "CompsTable", type = "html", out = "draftcomps1.doc", 
          report = "vct*", star.cutoffs = c(.05), order=c("DPI", "Isq"), 
          keep.stat = c("rsq", "adj.rsq", "f", "wald", "n", "ser"),
          float.env = "sidewaystable")
#regression plots for each
ggplot(canm5)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm26)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm31)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm33)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm35)+geom_point(aes(x=.fitted, y=.resid))
ggplot(canm36)+geom_point(aes(x=.fitted, y=.resid))


#scatterplots

#Printing scatterplots for each variable against the dependent
ScatterComps <- function(canm1df, na.rm = TRUE) {
  nm <- names(canm1df)
  for (i in seq_along(nm)) {
    plots <- ggplot(canm1df, aes_string(x = nm[i], y = REARN)) + geom_point()
    ggsave (plots, filename = paste("myplot", nm[i], ".png", sep = ""))
  }
}
ScatterComps(canm1df)

#durbin watson
dwtest(canm35)

#more quadratic equation
PLsq <- PL^2

canm37 <- lm(REARN ~ DPI + DPIsq + LFP + lnFLT + COV + PL + PLsq + COT +
               JAN + FEB + MAR + APR + MAY + 
               JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm37)
#wrecks everything

canm38 <- lm(REARN ~ DPI + DPIsq + LFP + lnFLT + COV + lnPL + COT +
               JAN + FEB + MAR + APR + MAY + 
               JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm38)

#finding license turning point
LICsq = LIC^2
DPIsq = DPI^2
canm39 <- lm(REARN ~   DPI + DPIsq + LFP + lnFLT + COV + COT + LIC + LICsq + PHO +
               TEMP + JAN + FEB + MAR + APR + MAY + 
               JUN + JUL + AUG + SEP + OCT + NOV, data = CompsData)
summary(canm39)
#It was a better regression, so to better specify the model, I put non-correlated variables back in

corcanm39<-cor(data.frame(DPI, LFP, lnFLT, COV, COT, LIC, PHO,
                          TEMP, JAN, FEB, MAR, APR, MAY, 
                          JUN, JUL, AUG, SEP, OCT, NOV), use = "pairwise.complete.obs")
corrplot(corcanm39, method = 'number', addCoefasPercent = TRUE, col = "black")
corcanm
dwtest(canm39)
ggplot(canm39)+geom_point(aes(x=.fitted, y=.resid))

#summary statistics
summary(CompsData)

sapply(CompsData, min, max, mean, sd)
install.packages('pastecs')
stat.desc(CompsData)
install.packages('psych')
describe(CompsData)

#presentation table / other presentation
stargazer((canm2), 
          title = "CompsTable", type = "html", out = "compsprez2.doc", 
          report = "vct*", star.cutoffs = c(.05), order=c("DPI", "Isq"), 
          keep.stat = c("rsq", "adj.rsq", "f", "wald", "n", "ser"),
          float.env = "sidewaystable")

#Final comps tables, table 3 name doesn't need to be changed
stargazer(list(canm29, canm33, canm35, canm36, canm39), 
          title = "CompsTable", type = "html", out = "compsfinal3.doc", 
          report = "vct*", star.cutoffs = c(.05), order=c("DPI", "Isq"), 
          keep.stat = c("rsq", "adj.rsq", "f", "wald", "n", "ser"),
          float.env = "sidewaystable")
stargazer(list(canm2, canm3, canm4), 
          title = "CompsTable", type = "html", out = "compsfinal4.doc", 
          report = "vct*", star.cutoffs = c(.05), order=c("DPI", "Isq"), 
          keep.stat = c("rsq", "adj.rsq", "f", "wald", "n", "ser"),
          float.env = "sidewaystable")
#Finding outliers
qqPlot(canm39,labels=row.names(Date), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
outlierTest(canm39)
cutoff <- 4/(nrow()-length(canm39$coefficients)-2)
plot(canm39, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col='red')

avPlots(canm39, ask=FALSE, id.method='identify')
influencePlot(canm39, id.method='identify', main='Influence Plot', 
              sub = 'Circle size proportionate to Cooks distance')

#Performing extrapolations
canm39exp <- data.frame(predict(canm39))
canm39exp
write.table(canm39exp, "canm39exp.txt")

predict(canm39)

attach(CompsData)
newm39DATA <- data.frame(REARN, DPI, DPIsq, LFP, lnFLT, COV, COT, 
                         LIC, LICsq,PHO,TEMP, JAN, FEB, MAR, 
                         APR, MAY, JUN, JUL, AUG, SEP, 
                         OCT, NOV)
newm39DATA
n39DATAshort <- newm39DATA[3:78, 1:22]
n39DATAshort
newREARN <- CompsData$REARN[3:78]
CompsData$REARN
newREARN
n2REARN <- CompsData$REARN[79:82]
CIpred <- predict(canm39, newdata = n39DATAshort,
                  interval = "confidence", ci = 0.95)
CIpred
write.table(CIpred, "canm39exp.txt")
write.csv(CIpred, "canm39exp.csv")
summary(canm39)

p1 <- ggplot(canm37)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm37 Residual Plot")
p2 <- ggplot(canm38)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm38 Residual Plot")
p3 <- ggplot(canm39)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm39 Residual Plot")
p4 <- ggplot(canm31)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm31 Residual Plot")
p5 <- ggplot(canm32)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm32 Residual Plot")
p6 <- ggplot(canm33)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm33 Residual Plot")
p7 <- ggplot(canm34)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm34 Residual Plot")
p8 <- ggplot(canm35)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm35 Residual Plot")
p9 <- ggplot(canm36)+geom_point(aes(x=.fitted, y=.resid)) + ggtitle("canm36 Residual Plot")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3)

