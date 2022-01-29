library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(rpart)
library(mgcv)
library(glmnet)
library(boot)
library(rpart.plot)

#loading data
data(Boston)
dim(Boston)

#a look at first few rows
head(Boston)

#a look at structure of the data set
glimpse(Boston)

#summary statistics
summary(Boston)

#Check for missing values
sum(is.na(Boston))

#Check for duplicated values
sum(duplicated(Boston))

mahal = mahalanobis(Boston[ ,], 
                    colMeans(Boston[ , ], 
                             na.rm = TRUE),
                    cov(Boston[ , ], 
                        use="pairwise.complete.obs"))
cutoff = qchisq(.999,ncol(Boston[ , ])) 
summary(mahal < cutoff)
Boston[ mahal > cutoff, ]
noout = Boston[ mahal < cutoff, ]



#checking correlation between variables
corrplot(cor(Boston), method = "number", type = "upper", diag = FALSE)



set.seed(123)
index <- sample(nrow(Boston), nrow(Boston) * 0.80)
Boston.train <- Boston[index, ]
Boston.test <- Boston[-index, ]

train.control <- trainControl(method = "cv", number = 10)

# Build the model
model <- train(medv ~ ., data = Boston.train,  method = "glm",
               trControl = train.control)
print(model)
summary(model)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(Boston.test)
(model %>% predict(Boston.test))



par(mfrow = c(1,1))

#get list of residuals 
res <- resid(model)
#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 


#Create density plot of residuals
plot(density(res))


holdout <- Boston[2,]
predictions2 <- model %>% predict(holdout)
(model %>% predict(holdout))


library(randomForest)
library(mlbench)
library(caret)

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
mtry <- sqrt(ncol(Boston))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(medv ~ ., data = Boston.train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)


tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(medv ~ ., data = Boston.train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
