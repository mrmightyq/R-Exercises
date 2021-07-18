# Regression Variable Transformations

# Outline:
#   Regression with quadratic term
#   Regression with interaction term
#   Regression with rescaled variables
#   Regression with logged rescaled variables

# Data files:
#   wage1.csv
#   CEOSAL2.csv

library(readr)
wage1 <- read_csv("C:/Users/KnudseQ/Desktop/R Data/wage1.csv")

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# Regression with quadratic term ---------------------------------------

# Wage example


# Regression
model_1 <- lm(wage ~ educ, wage1)
summary(model_1)
wage1 %<>% mutate(wagehat1 = fitted(model_1))
ggplot(data = wage1, mapping = aes(x = educ)) + 
  theme_bw() +
  geom_point(mapping = aes(y = wage, col = 'wage')) +
  geom_point(mapping = aes(y = wagehat1, col = 'linear prediction'))

# Regression with quadratic term
# wage = beta0 + beta1*educ + beta2*educsq + u
wage1 %<>% mutate(educsq = educ^2)
model_2 <- lm(wage ~ educ + educsq, wage1)
summary(model_2)
wage1 %<>% mutate(wagehat2 = fitted(model_2))
ggplot(data = wage1, mapping = aes(x = educ)) + 
  theme_bw() +
  geom_point(mapping = aes(y = wage, col = 'wage')) +
  geom_point(mapping = aes(y = wagehat2, col = 'linear prediction'))

# Calculate min or max point for partial effect, educ*=-beta1/2*beta2
b_2      <- coef(model_2)
b_educ   <- b_2["educ"]
b_educsq <- b_2["educsq"]
-b_educ / (2*b_educsq)

# Partial effect of educ on wage = beta1 + 2*beta2*educ
b_educ + 2*b_educsq*5
b_educ + 2*b_educsq*6.18
b_educ + 2*b_educsq*10
b_educ + 2*b_educsq*15

# Calculate partial effect at the mean
(mean_educ <- mean(wage1$educ))
(pem_educ <- b_educ + 2*b_educsq*mean_educ)

# Calculating average partial effect
pe_educ  <- b_educ + 2*b_educsq*wage1$educ
(ape_educ <- mean(pe_educ))


# Example with experience instead of education

# Regression 
model_3 <- lm(wage ~ exper, wage1)
summary(model_3)
wage1 %<>% mutate(wagehat3 = fitted(model_3))
ggplot(data = wage1, mapping = aes(x = exper)) + 
  theme_bw() +
  geom_point(mapping = aes(y = wage, col = 'wage')) +
  geom_point(mapping = aes(y = wagehat3, col = 'linear prediction'))

# Regression with quadratic term
# wage = beta0 + beta1*exper + beta2*expersq + u
# expersq is given in dataset
model_4 <- lm(wage ~ exper + expersq, wage1)
summary(model_4)
wage1 %<>% mutate(wagehat4 = fitted(model_4))
ggplot(data = wage1, mapping = aes(x = exper)) + 
  theme_bw() +
  geom_point(mapping = aes(y = wage, col = 'wage')) +
  geom_point(mapping = aes(y = wagehat4, col = 'linear prediction'))

# Calculate min or max point for partial effect, exper*=-beta1/2*beta2
b_4 <- coef(model_4)
b_exper <- b_4["exper"]
b_expersq <- b_4["expersq"]
-b_exper / (2*b_expersq)

# Partial effect of exper on wage = beta1 + 2*beta2*exper
b_exper + 2*b_expersq*10
b_exper + 2*b_expersq*20
b_exper + 2*b_expersq*24.3
b_exper + 2*b_expersq*30

# Calculating partial effect at the mean
(mean_exper <- mean(wage1$exper))
(pem_exper <- b_exper + 2*b_expersq*mean_exper)

# Calculating average partial effect
pe_exper <- b_exper + 2*b_expersq*wage1$exper
(ape_exper <-  mean(pe_exper))


# Regression with interaction term --------------------------------------
  
# Regression
model_5 <- lm(wage ~ educ + exper + tenure, wage1)
summary(model_5)
wage1 %<>% mutate(wagehat5 = fitted(model_5))
ggplot(wage1, aes(x = educ)) +
  theme_bw() +
  geom_point(aes(y = wage, col = 'wage')) +
  geom_point(aes(y = wagehat5, col = 'linear predictor'))

# Generate interaction term
wage1 %<>% mutate(educXexper = educ*exper,
                  experXtenure = exper*tenure)

# Regression with interaction term
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + beta4*educ*exper
model_6 <- lm(wage ~ educ + exper + tenure + educXexper, wage1)
summary(model_6)
wage1 %<>% mutate(wagehat6 = fitted(model_6))
ggplot(wage1, aes(x = educ)) +
  theme_bw() +
  geom_point(aes(y = wage, col = 'wage')) +
  geom_point(aes(y = wagehat6, col = 'linear predictor'))

# Calculate partial effect of education on wage at several levels of experience
# = beta1 + beta4*exper
b_6 <- coef(model_6)
b_educ <- b_6["educ"]
b_educexper <- b_6["educXexper"]

b_educ + b_educexper*10
b_educ + b_educexper*17
b_educ + b_educexper*30

# Regression with another interaction term
model_7 <- lm(wage ~ educ + exper + tenure + experXtenure, wage1)
wage1 %<>% mutate(wagehat7 = fitted(model_7))
ggplot(wage1, aes(x = exper)) +
  theme_bw() +
  geom_point(aes(y = wage, col = 'wage')) +
  geom_point(aes(y = wagehat7, col = 'linear predictor'))


# Regression with rescaled variables -----------------------------------

# CEO salary example
CEOSAL2 <- read.csv(paste0(directory, "CEOSAL2.csv"))

CEOSAL2 %<>% select(salary, sales, profits, lsalary, lsales)
str(CEOSAL2)
stargazer(CEOSAL2, type = "text")

# Rescale salary from thousands of dollars into dollars
CEOSAL2 %<>% mutate(salary_d = salary*1000)

# Rescale sales from millions of dollars to thousands of dollars
CEOSAL2 %<>% mutate(sales_k = sales*1000)

# Descriptive statistics
CEOSAL2 %>% 
  select(salary, salary_d, sales, sales_k, profits) %>%
  stargazer(type = "text", digits = 0)

# Regressions with original and rescaled variables
lm(salary   ~ sales   + profits, CEOSAL2) %>% summary
lm(salary_d ~ sales   + profits, CEOSAL2) %>% summary
lm(salary   ~ sales_k + profits, CEOSAL2) %>% summary
lm(salary_d ~ sales_k + profits, CEOSAL2) %>% summary
# When variables are rescaled, the coefficients are rescaled.


# Regression with logged rescaled variables ----------------------------

# Regressions with log transformation

# Level-level regression
lm(salary ~ sales + profits, CEOSAL2) %>% summary
# Log-level regression
lm(lsalary ~ sales + profits, CEOSAL2) %>% summary
# Level-log regression
lm(salary ~ lsales + profits, CEOSAL2) %>% summary
# Log-log regression
lm(lsalary ~ lsales + profits, CEOSAL2) %>% summary

# Rescale salary from thousands of dollars into dollars
CEOSAL2 %<>% mutate(lsalary_d = log(salary_d))
# Rescale sales from millions of dollars to thousands of dollars
CEOSAL2 %<>% mutate(lsales_k = log(sales_k))

# Descriptive statistics
CEOSAL2 %>% 
  select(lsalary, lsalary_d, lsales, lsales_k, profits) %>%
  stargazer(type = "text", digits = 1)

# Regressions with original logged variables and rescaled logged variables
lm(lsalary   ~ lsales   + profits, CEOSAL2) %>% summary
lm(lsalary_d ~ lsales   + profits, CEOSAL2) %>% summary
lm(lsalary   ~ lsales_k + profits, CEOSAL2) %>% summary
lm(lsalary_d ~ lsales_k + profits, CEOSAL2) %>% summary
# When logged variables are rescaled, the coefficients do not change.
