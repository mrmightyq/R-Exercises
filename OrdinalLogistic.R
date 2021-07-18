# 1. Data
library(readr)
Cardiotocographic <- read_csv("C:/Users/KnudseQ/Desktop/R Data/Cardiotocographic.csv")
data <- Cardiotocographic
str(data)
data$NSP <- as.ordered(data$NSP)
data$Tendency <- as.factor(data$Tendency)
summary(data)
xtabs(~NSP+Tendency,data)

# 2. Partition data
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# 3. Ordinal Logitic Regression or Proportional Odds Logistic Regression
library(MASS)
model <- polr(NSP~ .-Max-LB-MSTV-Nmax-Nzeros-Median-FM-MLTV, train, Hess = TRUE)
summary(model)

# 4. p-Value Calculation
(ctable <- coef(summary(model)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# 5. Prediction
pred <- predict(model, train)
print(pred, digits = 3)

# 6. Confusion Matrix & Error for Training data
(tab <- table(pred,train$NSP))
1-sum(diag(tab))/sum(tab)

# 7. Confusion Matrix & Error for Test data
pred1 <- predict(model,test)
(tab1 <- table(pred1, test$NSP))
1-sum(diag(tab1))/sum(tab1)

# End


