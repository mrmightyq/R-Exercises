# if needed, download health_insurance data
url <- "http://peopleanalytics-regression-book.org/data/health_insurance.csv"
health_insurance <- read.csv(url)

# view first few rows
head(health_insurance)

# view structure
str(health_insurance)

library(GGally)

# convert product and gender to factors
health_insurance$product <- as.factor(health_insurance$product)
health_insurance$gender <- as.factor(health_insurance$gender)

GGally::ggpairs(health_insurance)



library(dummies)

# create dummies for product choice outcome
dummy_product <- dummies::dummy("product", data = health_insurance)

# combine to original set
health_insurance <- cbind(health_insurance, dummy_product)

# run a binomial model for the Product A dummy against 
# all input variables (let glm() handle dummy input variables)
A_model <- glm(
  formula = productA ~ age + gender + household + 
    position_level + absent, 
  data = health_insurance, 
  family = "binomial"
)


# summary
summary(A_model)




# simpler model
A_simple <- glm(
  formula = productA ~ age + household + gender + position_level, 
  data = health_insurance
)

# view odds ratio as a data frame
as.data.frame(exp(A_simple$coefficients))


#define reference by ensuring it is the first level of the factor
health_insurance$product <- relevel(health_insurance$product, ref = "A")

# check that A is now our reference
levels(health_insurance$product)


library(nnet)

multi_model <- multinom(
  formula = product ~ age + gender + household + 
    position_level + absent, 
  data = health_insurance
)

summary(multi_model)



# calculate z-statistics of coefficients
z_stats <- summary(multi_model)$coefficients/
  summary(multi_model)$standard.errors

# convert to p-values
p_values <- (1 - pnorm(abs(z_stats)))*2


# display p-values in transposed data frame
data.frame(t(p_values))


# display odds ratios in transposed data frame
odds_ratios <- exp(summary(multi_model)$coefficients)
data.frame(t(odds_ratios))

# calculate difference between coefficients and view as column
coefs_c_to_b <- summary(multi_model)$coefficients[2, ] - 
  summary(multi_model)$coefficients[1, ]

data.frame(coefs_c_to_b)


# remove absent
simpler_multi_model <- multinom(
  formula = product ~ age + gender + household + position_level,
  data = health_insurance, 
  model = TRUE
)


# view coefficients with absent
data.frame(t(summary(multi_model)$coefficients))


# view coefficients without absent
data.frame(t(summary(simpler_multi_model)$coefficients))



DescTools::PseudoR2(simpler_multi_model, 
                    which = c("McFadden", "CoxSnell", "Nagelkerke"))
