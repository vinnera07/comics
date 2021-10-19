
#[Block 1. Import the data and upload libraries]
library(dplyr)
library(ggthemes)
library(ggplot2)
library(car)
library(cowplot)
library(stargazer)
library(interplot)
library(interactions)

dec52 <- read.csv("C:/Users/Лера Ника/Desktop/григорчик/DEC52.csv", sep=';')

##[Block 2. Data filtration and cleansing]

#transform variables into numeric
dec52$Number_of_voters <- as.numeric(dec52$Number_of_voters)
dec52$Early_voting <- as.numeric(dec52$Early_voting)
dec52$Absentee_Ballots <- as.numeric(dec52$Absentee_Ballots)
dec52$Invalid_votes <- as.numeric(dec52$Invalid_votes)
dec52$Station_Ballots <- as.numeric(dec52$Station_Ballots)

#getting rid of % and tranforming the rest varibles(parties ex-%) as well
DEC52 <- dec52 %>% dplyr::select(Rodina, KR, RPPS, United_Russia, Zelenye, GP, LDPR, PARNAS, 
                                     Partiia_Rosta, GS, Yabloko, KPRF, PR, SR)

for(i in 1:ncol(DEC52)){ DEC52[,i] <- as.numeric(sub("%", "",DEC52[,i],fixed=TRUE))/100 }

#calculating Voter Turnout
dec52$Turnout = (dec52$Early_voting + dec52$Station_Ballots + dec52$Absentee_Ballots)/dec52$Number_of_voters
Turnout = dec52$Turnout

#calculating share of Invalid Votes
dec52$Invalid_share = dec52$Invalid_votes/dec52$Number_of_voters
Invalid_share = dec52$Invalid_share

#binding data and rounded it to a thousandth
Data = DEC52 %>% cbind(Turnout, Invalid_share)
Data = Data %>% round(digits = 3) 

Comission = dec52$Comission
Data = Data  %>% cbind(Comission)


#[Block 3. Script for building visualizations]
attach(Data)
#1
#histogram of Turnout Distribution
hist(Turnout,freq=F,breaks=70,
     main = "2016 Duma Elections\nDistribution of Turnout Percentage",
     xlab = "Turnout",
     ylab = "Density",
     border="black", col="#AFEEEE")
lines(density(Turnout), col="black")

#histogram of Votes for the United Russia Distribution
hist(United_Russia,freq=F,breaks=70,
     main = "2016 Duma Elections\nDistribution of the Votes for\nthe 'United Russia'",
     xlab = "UR's share of votes",
     ylab = "Density",
     border="black", col="#AFEEEE")
lines(density(United_Russia), col="black")
par(mfrow=c(1,2))

#2
Data$small_parties <- Data$Rodina + Data$Yabloko + Data$RPPS +  Data$Zelenye + Data$GP + Data$GS + Data$PARNAS + Data$Partiia_Rosta + Data$PR

ggplot(Data) + geom_boxplot(aes(x='United Russia', y = United_Russia), fill = "Pink", col = "red") + 
  ggtitle("2016 Duma elections:\n Votes for parliamentary parties") + 
  xlab("Parties") + ylab("Distribution") +
  geom_boxplot(aes(x='KPRF', y = KPRF), col = "#00008B", fill = "#87CEFA") + 
  geom_boxplot(aes(x='LDPR', y = LDPR), col = "#00008B", fill = "#87CEFA") + 
  geom_boxplot(aes(x='A Just Russia', y = SR), col = "#00008B", fill = "#87CEFA") + 
  geom_boxplot(aes(x='Communist Party of Russian Communists', y = KR), fill = "#87CEFA", col = "#00008B") + 
  geom_boxplot(aes(x='Small parties', y = small_parties), col = "#00008B", fill = "#87CEFA")

#3
UR_plot <- ggplot(Data) + 
  aes(y=United_Russia, x=Turnout) +
  geom_point() +
  xlab("Turnout")+
  ylab("Share of Votes: United Russia")+
  ggtitle("Dependence of United Russia\nvotes share from Turnout")+
  stat_smooth(method = "lm", se = T, col="red")
UR_plot

KPRF_plot = ggplot(Data) + 
  aes(y=KPRF, x=Turnout) +
  geom_point() +
  xlab("Turnout")+
  ylab("Share of votes: KPRF")+
  ggtitle("Dependence of KPRF\nvotes share from Turnout")+
  stat_smooth(method = "lm", se = T, col="orange")
KPRF_plot

LDPR_plot = ggplot(Data) + 
  aes(y=LDPR, x=Turnout) +
  geom_point() +
  xlab("Turnout")+
  ylab("Share of Votes: LDPR")+
  ggtitle("Dependence of LDPR\nvotes share from Turnout")+
  stat_smooth(method = "lm", se = T, col="yellow")
LDPR_plot

SR_plot = ggplot(Data) + 
  aes(y=SR, x=Turnout) +
  geom_point() +
  xlab("Turnout")+
  ylab("Share of votes: A Just Russia")+
  ggtitle("Dependence of A Just Russia\nvotes share from Turnout")+
  stat_smooth(method = "lm", se = T, col="green")+
  theme_bw()
SR_plot

KR_plot = ggplot(Data) + 
  aes(y=KR, x=Turnout) +
  geom_point() +
  xlab("Turnout")+
  ylab("Share of Votes: KR")+
  ggtitle("Dependence of Communist Party of Russian Communists\nvotes share from Turnout")+
  stat_smooth(method = "lm", se = T, col="blue")
KR_plot


plot_1 = plot_grid(UR_plot, KPRF_plot, LDPR_plot, SR_plot, KR_plot)
plot_1

#4
attach(Data)
#checking the existence of interection to see whether to do further tasks

interaction<-lm(United_Russia~Invalid_share*Turnout)
summary(interaction)

#There is a strong interaction between predictor and moderator (***), the p-value is less than 0.05 (2.82e-06). => build an interplot
interplot(m = interaction,
          var1 = "Invalid_share",
          var2 = "Turnout", hist = T) +
  xlab("Turnout") +
  ylab("Marginal effect of Invalid votes share on the share of votes for United Russia") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed")
#The Invalid_share (predictor) is under the effect of the Turnout (moderator) - DV (United_Russia) relationship.
#Since the trendline is under negative decline, the relationship has the negative direction.

#5
#There is a strong interaction between predictor and moderator (***), the p-value is less than 0.05 (2.82e-06). => build Johnson-Neyman interval plot
options(width = 60)
johnson_neyman(interaction,
               pred = "Invalid_share", modx = "Turnout", alpha = 0.05, control.fdr = T, plot = T)

#The interaction is significant beyond the interval [0.27, 0.44]
#Interpretation:
#The interaction effect exists only for the values of Turnout [0.24, 0.27) and (0.44, 1.00]. Between that intervals the p-value is greater than 0.05, and effect is insignificant.

#6 - Q-Q plot;
regression<-lm(United_Russia~Turnout+Invalid_share)
summary(regression)

qqPlot(regression, main="Q-Q Plot")#There is a problem of non-normal distribution of residuals, due to existence of ponits outside the boarders
summary(powerTransform(United_Russia))
#The p-value is less than 0.05 (0.00049748). => Transformation of the DV is necessary (the existent problem of non-normality of residuals distribution
#does hinder the model's result. => Correction is needed) 

#7
crPlots(regression)    
#There is a deviation from the blue trend =>  the problem of non-linearity

#Statistical analysis of the collected data

#[Block 4. Script for building descriptive statistics table]

stargazer(Data, type="html",
          title="Descriptive statistics",
          digits=3,
          summary.stat=c("n", "mean", "sd", "min", "max","median"),
          out="Descriptive statistics for QM_PW.htm")


#[Block 5. Script for building regression table]

stargazer(regression,
          type="html",
          model.numbers = FALSE,
          ci=TRUE,
          title="Regression results for United Russia",
          dep.var.labels=c("United Russia"),
          covariate.labels=c("Turnout", "Invalid Ballots"),
          out="Regression results for QM_PW.html")

#[Block 6. Script for OLS regression diagnostics]

#Multicollinearity problem
vif(regression)
#The VIF values are smaller than 10 (  1.000117 and 1.000117), hence there is ot any problem of multicollinearity

#Heteroscedasticity problem
ncvTest(regression) #P-value is smaller than 0.05 (3.0593e-16), hence there is heteroskedasticity in the model and it does hinder the model
spreadLevelPlot(regression) #the blue trend is under an inclination, hence the model does have the problem of heteroskedasticity

#Outliers
outlierTest(regression)
# There are 2 outliers in the model - lines 200  and 393, since their Bonferroni p are less than 0.05 (0.00028406 and 0.00036885).

#Influential observations
plot(regression, which=4, cook.levels=1)
abline(h=1, lty=2, col="blue")
#There is an influential obesravtion on the line 200, since its Cook's distance excceds 1.


