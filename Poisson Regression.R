library(data.table)
library(readr)
library(dplyr)
# Read Dummy Data 
data <- read_csv("C:/Users/KnudseQ/Desktop/R Data/Prussion Horse-Kick Data.csv")
summary(data)
#Melt to Long Format
long <- melt(setDT(data), id.vars = c("Year"), variable.name = "Cat")
x<-long %>% 
  group_by(Cat) %>%
  summarise(avg=mean(value))
x <- as.data.frame(x)
hist(x$avg)
library(ggplot2)

# Using median
long %>%
  mutate(Cat = fct_reorder(Cat, value, .fun='median')) %>%
  ggplot( aes(x=reorder(Cat, value), y=value, fill=Cat)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position="none") +
  xlab("")


library(dplyr)
long %>% 
  group_by(Cat) %>%
  summarise(avg=mean(value), sd = sd(value))

boxplot(value~Cat,data=long)


# Non melted 
mahal = mahalanobis(data[ , -c(2)], 
                    colMeans(data[ , -c(2)], 
                             na.rm = TRUE),
                    cov(data[ , -c(2)], 
                        use="pairwise.complete.obs"))
cutoff = qchisq(.999,ncol(data[ , -c(2)])) 
summary(mahal < cutoff)
data[ mahal > cutoff, ]
noout = data[ mahal < cutoff, ]



x %>%
  mutate(Cat = fct_reorder(Cat, desc(avg))) %>%
  ggplot(x, aes(factor(Cat), avg, fill = Cat)) + 
  geom_bar(stat="identity", position = "dodge")

# Confirm Poisson
hist(long$value)
# Poisson Regression
model1 <-glm(value ~ Cat, family = "poisson",
             data = long)
summary(model1)


mahal = mahalanobis(long[ , -c(2)], 
                    colMeans(long[ , -c(2)], 
                             na.rm = TRUE),
                    cov(long[ , -c(2)], 
                        use="pairwise.complete.obs"))
cutoff = qchisq(.999,ncol(long[ , -c(2)])) 
summary(mahal < cutoff)
long[ mahal > cutoff, ]
noout = long[ mahal < cutoff, ]

boxplot(noout$value)

x<- boxplot(long$value)
outliers_values=x$out

new_data=long[!(long %in% outliers_values)]







library(tidyverse)
library(caret)
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(value ~ Cat, data = long,  method = "glmnet",
               family = "poisson",
               trControl = train.control)
# Summarize the results
model <- train(value ~ Cat, data = long,  method = "glm",
               family = "poisson",
               trControl = train.control)
print(model)
summary(model)


set.seed(123)
training.samples <- long$value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- long[training.samples, ]
test.data <- long[-training.samples, ]

# Build the model
model <- train(value ~ Cat, data = train.data,  method = "glm",
                family = "poisson",
                trControl = train.control)
print(model)
summary(model)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
(model %>% predict(test.data))



test <- data.frame(Year=c(1888),
                          Cat=c('C15'),
                          value=c(2))
test
(model %>% predict(test))
