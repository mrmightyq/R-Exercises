##dr b's working directory
#setwd("~/OneDrive - Missouri State University/TEACHING/527 Adv Statistics/2017 - Blend/1 notes/11 Med Mod")

##import the file
library(readr)
#sustain <- read_csv("C:/Users/KnudseQ/Desktop/sustain.csv")
data <- sustain 
library(dplyr)
sustain <- read_csv("B:/GLOBAL/2063-BASUS/FLORHAM-PARK/NTH/NTH-T/TalentManagement/03_Data & Analytics (Quinn & Jake)/Quinn/Sustainability Educate and Engage Project/Sustainability Check-in (1-163).csv")


sustain2 <- sustain %>% 
  mutate_at(.vars = vars(10:17), 
            .funs = function(x) recode(x, 
                                       "Strongly Agree" = 5, 
                                       "Agree" = 4, 
                                       "Neutral" = 3, 
                                       "Disagree" = 2, 
                                       "Strongly Disagree" = 1))
df <- sustain2[ -c(1:9, 18:20) ]


####polytomous IRT####
polydata = na.omit(df)
CFA_Data<-scale(polydata,center=TRUE,scale=TRUE)
CFA_Data<-as.data.frame(CFA_Data)


#CFA_Data %>% 
# rename(
#  `I have taken action to help BASF and/or our customers advance on sustainability within the past 6 months.` = ACT1,
# `Sustainability directly impacts BASF’s market growth and long-term success.` = PUR1,
#`I am proud of BASF’s commitment to sustainability.` = PUR2,
#`My direct manager speaks about BASF’s sustainability priorities frequently.` =LEAD1,
#`I feel my direct manager's actions show support for sustainability.`=LEAD2,
#`I am motivated by my direct manager’s commitment to sustainability.`=LEAD3,
#`I would recommend BASF as an employer based upon its commitment to sustainability.`=PUR3,
#`I am confident discussing and taking action on sustainability at work.`=ACT2
#)

names(CFA_Data)[1] <- "ACT1"
names(CFA_Data)[2] <- "PUR1"
names(CFA_Data)[3] <- "PUR2"
names(CFA_Data)[4] <- "LEAD1"
names(CFA_Data)[5] <- "LEAD2"
names(CFA_Data)[6] <- "LEAD3"
names(CFA_Data)[7] <- "PUR3"
names(CFA_Data)[8] <- "ACT2"

master = CFA_Data

##accuracy
summary(master)

##run the final model to get assumption checks
output = lm(ACT1 ~ scale(PUR2, scale = F) * scale(LEAD1, scale = F), 
            data = master)

##outliers
##mahal
mahal = mahalanobis(master,
                    colMeans(master),
                    cov(master))
cutoff = qchisq(1-.001, ncol(master))
cutoff ##cutoff score
ncol(master) ##df
badmahal = as.numeric(mahal > cutoff) ##notice the direction of > 
table(badmahal)

##leverage
k = 2 ##number of IVs
leverage = hatvalues(output)
cutleverage = (2*k+2) / nrow(master)
cutleverage ##cut off
badleverage = as.numeric(leverage > cutleverage)
table(badleverage)

##cooks
cooks = cooks.distance(output)
cutcooks = 4 / (nrow(master) - k - 1)
cutcooks ##get the cut off
badcooks = as.numeric(cooks > cutcooks)
table(badcooks)

##overall outliers
##add them up!
totalout = badmahal + badleverage + badcooks
table(totalout)

##get rid of them!
noout = subset(master, totalout < 2)

##run a no outlier analysis otherwise these graphs
##will include the outliers
output = lm(ACT1 ~ scale(PUR2, scale = F) * scale(LEAD1, scale = F), 
            data = noout)

##additivity not necessary bec we have simple moderation 
##correl = cor(noout, use="pairwise.complete.obs")
##correl
##symnum(correl)

##assumption set up
standardized = rstudent(output)
fitted = scale(output$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity and homoscedasticity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##moderation 
library(QuantPsyc)

##run the regression
model = moderate.lm(LEAD1, PUR2, ACT1, noout)
summary(model)

##run the post hoc simple slopes
simple = sim.slopes(model, meanCenter(noout$LEAD1))
simple

mean(noout$LEAD1)
sd(noout$LEAD1)

# use mouse click to place legend in graph.mod
graph.mod(simple,PUR2,ACT1,master,
          xlab = "Centered Books Read",
          ylab = "Grade in Class")

library(rockchalk)
ps <- plotSlopes(model, plotx="mcx", modx="mcz", modxVals="std.dev")
ts <- testSlopes(ps)
plot(ts)



##mediation analysis
modelc = lm(innings ~ league, data = noout)
summary(modelc)

modela = lm(budget ~ league, data = noout)
summary(modela)

modelb = lm(innings ~ league + budget, data = noout)
summary(modelb)

##sobel
library(multilevel)
sobel(noout$league, noout$budget, noout$innings)
save = sobel(noout$league, noout$budget, noout$innings)
pnorm(abs(save$z.value), lower.tail = F)*2

##interpretation for dummy coded variables
tapply(noout$innings, list(noout$league), mean)
tapply(noout$budget, list(noout$league), mean)