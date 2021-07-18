#Installing the Psych package and loading it
install.packages("psych")
library(psych)
#Loading the dataset
bfi_data=bfi
#Remove rows with missing values and keep only complete cases
bfi_data=bfi_data[complete.cases(bfi_data),]
#Create the correlation matrix from bfi_data
bfi_cor <- cor(bfi_data[,1:25])
#Factor analysis of the data
factors_data <- fa(r = bfi_cor, nfactors = 5)
#Getting the factor loadings and model analysis
factors_data
#Test for optimal number of factors
parellel <- fa.parallel(bfi_data[,1:25],fm="ml",fa="fa")


round1 = fa(bfi_data[,1:25], nfactors=6, rotate = "oblimin", fm = "ml")
round1
finalmodel <- round1
#Calculate the CFI
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))





##import the file
master = read.csv("14 EFA data.csv")

##accuracy
summary(master)

##recode
table(master$q6)
master[ , c(6,9,17,27)] = 8 - master[ , c(6,9,17,27)]
table(master$q6)

##missing
percentmissing = function (x){ sum(is.na(x))/length(x) * 100}
missing = apply(master, 1, percentmissing)
table(missing)

##exclude the participant missing too much data
replacepeople = subset(master, missing <= 5)

##make sure the columns aren't missing too much
apply(replacepeople, 2, percentmissing)

##replace away!
library(mice)
tempnomiss = mice(replacepeople)
nomiss = complete(tempnomiss, 1)
summary(nomiss)

##outliers
cutoff = qchisq(1-.001, ncol(nomiss))
mahal = mahalanobis(nomiss,
                    colMeans(nomiss),
                    cov(nomiss))
cutoff ##cutoff score
ncol(nomiss) ##df
summary(mahal < cutoff)

##exclude outliers
noout = subset(nomiss, mahal < cutoff)

##additivity
correl = cor(noout, use = "pairwise.complete.obs")
symnum(correl)
correl

##assumption set up
random = rchisq(nrow(noout), 7)
fake = lm(random~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)

##normality
hist(standardized)

##linearity
qqnorm(standardized)
abline(0,1)

##homogeneity
plot(fitted,standardized)
abline(0,0)
abline(v = 0)

##running the efa analysis
library(psych)
library(GPArotation)

##correlation adequacy Bartlett's test
cortest.bartlett(correl, n = nrow(noout))

##sampling adequacy KMO test
KMO(correl)

##how many factors?
nofactors = fa.parallel(noout, fm="ml", fa="fa")
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a three factor model
round1 = fa(noout, nfactors=3, rotate = "oblimin", fm = "ml")
round1

round2 = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
round2

##get cfi
finalmodel = fa(noout[ , -c(4,15)], nfactors=3, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1, 3, 7, 8, 10:12, 14, 16, 18, 20:25, 29, 31, 32)
factor2 = c(2, 5, 13, 19, 26, 28, 30)
factor3 = c(6, 9, 17, 27)
psych::alpha(noout[ , factor1])
psych::alpha(noout[ , factor2])
psych::alpha(noout[ , factor3])

##create new factor scores
noout$f1 = apply(noout[ , factor1], 1, mean) ##creates average scores
noout$f2 = apply(noout[ , factor2], 1, mean) ##creates average scores
noout$f3 = apply(noout[ , factor3], 1, mean) ##creates average scores

summary(noout)
sd(noout$f1)
sd(noout$f2)
sd(noout$f3)


##install the things you don't have
install.packages("car")
install.packages("psych")
install.packages("GPArotation")


##load the data
efadata = read.csv("c17 efa raq.csv")

##libraries
library(psych)
library(GPArotation)
library(car)

##be sure to reverse score your data
##you will have to do each item one at a time
table(efadata$Q03)
efadata$Q03 = recode(efadata$Q03, "1=5; 2=4; 3=3; 4=2; 5=1")
table(efadata$Q03)

##normal data screening (fake style) goes here
##screen all the items (but not demographics)

##correlation adequacy Bartlett's test
correlations = cor(efadata)
cortest.bartlett(correlations, n = nrow(efadata))

##sampling adequacy KMO test
KMO(correlations)

##how many factors?
nofactors = fa.parallel(efadata, fm="ml", fa="fa")
nofactors$fa.values
sum(nofactors$fa.values > 1.0) ##old kaiser criterion
sum(nofactors$fa.values > .7) ##new kaiser criterion

##simple structure with a two factor model
fa(efadata, nfactors=2, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")

##get cfi
finalmodel = fa(efadata[ , -c(23)], nfactors=2, rotate = "oblimin", fm = "ml")
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

##reliability
factor1 = c(1:7, 9:10, 12:16, 18:22)
factor2 = c(8, 11, 17)
alpha(efadata[, factor1])
alpha(efadata[, factor2])

##simple structure with a five factor model
##an example of OVERfactoring
fa(efadata, nfactors=5, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(15)], nfactors=5, rotate = "oblimin", fm = "ml")
##all items load but factor five only has two items
##try four factor model
fa(efadata[ , -c(15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,14,15)], nfactors=4, rotate = "oblimin", fm = "ml")
fa(efadata[ , -c(3,7,10,14,15,18)], nfactors=4, rotate = "oblimin", fm = "ml")
##at this point you would get rid of 12, and then factor four
##only has two items ... this pattern indicates that you should 
##try the smaller numbers of factors