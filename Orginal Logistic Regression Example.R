library(carData)
library(MASS)
data(WVS) 
head(WVS)
summary(WVS)
ggplot(WVS, aes(x = poverty, y = age, fill = poverty)) +  
  geom_boxplot(size = .75) +   facet_grid(country ~ gender, margins = FALSE) +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
model_fit <- polr(poverty~religion+degree+country+age+gender, data = WVS, Hess = TRUE)
summary(model_fit)
summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table
new_data <- data.frame("religion"= "yes","degree"="no","country"="Norway","age"=30,"gender"="male")
round(predict(model_fit,new_data,type = "p"), 3)



# install peopleanalyticsdata package
#install.packages("peopleanalyticsdata")
library(peopleanalyticsdata)

# see a list of data sets
data(package = "peopleanalyticsdata")

# find out more about a specific data set ('managers' example)
help(managers)

# if needed, download data
url <- "http://peopleanalytics-regression-book.org/data/soccer.csv"
soccer <- read.csv(url)
head(soccer)
str(soccer)
# convert discipline to ordered factor
soccer$discipline <- ordered(soccer$discipline, 
                             levels = c("None", "Yellow", "Red"))

# check conversion
str(soccer)


# apply as.factor to four columns
cats <- c("position", "country", "result", "level")
soccer[ ,cats] <- lapply(soccer[ ,cats], as.factor)

# check again
str(soccer)

# run proportional odds model
library(MASS)
model <- polr(
  formula = discipline ~ n_yellow_25 + n_red_25 + position + 
    country + level + result, 
  data = soccer
)

# get summary
summary(model)


# get coefficients (it's in matrix form)
coefficients <- summary(model)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2

# bind back to coefficients
(coefficients <- cbind(coefficients, p_value))

# calculate odds ratios
odds_ratio <- exp(coefficients[ ,"Value"])
# combine with coefficient and p_value
(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))
head(fitted(model))
# diagnostics of simpler model
library(DescTools)
DescTools::PseudoR2(
  model, 
  which = c("McFadden", "CoxSnell", "Nagelkerke", "AIC")
)
# lipsitz test 
generalhoslem::lipsitz.test(model)
## 
# pulkstenis-robinson test 
# (requires the vector of categorical input variables as an argument)
generalhoslem::pulkrob.chisq(model, catvars = cats)
# create binary variable for "Yellow" or "Red" versus "None"
soccer$yellow_plus <- ifelse(soccer$discipline == "None", 0, 1)

# create binary variable for "Red" versus "Yellow" or "None"
soccer$red <- ifelse(soccer$discipline == "Red", 1, 0)
# model for at least a yellow card
yellowplus_model <- glm(
  yellow_plus ~ n_yellow_25 + n_red_25 + position + 
    result + country + level, 
  data = soccer, 
  family = "binomial"
)

# model for a red card
red_model <- glm(
  red ~ n_yellow_25 + n_red_25 + position + 
    result + country + level,
  data = soccer, 
  family = "binomial"
)

(coefficient_comparison <- data.frame(
  yellowplus = summary(yellowplus_model)$coefficients[ , "Estimate"],
  red = summary(red_model)$coefficients[ ,"Estimate"],
  diff = summary(red_model)$coefficients[ ,"Estimate"] - 
    summary(yellowplus_model)$coefficients[ , "Estimate"]
))


library(brant)
brant::brant(model)
