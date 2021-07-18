list.of.packages <- c("data.table", "tableone", "jtools", "ggstance", "MatchIt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


load(url("https://drive.google.com/uc?id=1SrnmBN2GE6H3sRDsRTyEUtrIioAbVxZ1"))
analytic.data <- DT9a
dim(analytic.data)


ps.formula <- as.formula(I(arthritis.type==
                             "Rheumatoid arthritis") ~ gender+bmi+
                           diabetes+smoke+age+race+born+education+
                           marriage+annualincome+physical.activity+
                           medical.access+blood.pressure+healthy.diet+
                           covered.health)
# fit logistic regression
PS.fit <- glm(ps.formula,family="binomial", data=analytic.data)
summary(PS.fit)
# extract propensity scores
analytic.data$PS <- predict(PS.fit, type="response")
# summarize propensity scores
summary(analytic.data$PS)


# plot propensity scores by exposure group
plot(density(analytic.data$PS[analytic.data$arthritis.type==
                                "Non-arthritis"]), col = "red", main = "")
lines(density(analytic.data$PS[analytic.data$arthritis.type==
                                 "Rheumatoid arthritis"]), col = "blue", lty = 2)
legend("topright", c("Non-arthritis","Rheumatoid arthritis"), 
       col = c("red", "blue"), lty=1:2)


ps.formula <- as.formula(I(arthritis.type==
                             "Rheumatoid arthritis") ~ gender+bmi+
                           diabetes+smoke+age+race+born+education+
                           marriage+annualincome+physical.activity+
                           medical.access+blood.pressure+healthy.diet+
                           covered.health)

install.packages("Rcpp")
install.packages("MatchIt")
library(MatchIt)
set.seed(123)
# This function fits propensity score model (using logistic 
# regression as above) when specified distance = 'logit'
# performs nearest-neighbor (NN) matching, 
# without replacement 
# with caliper = .2*SD of propensity score  
# within which to draw control units 
# with 1:1 ratio (pair-matching)
match.obj <- matchit(ps.formula, data = analytic.data,
                     distance = 'logit', 
                     method = "nearest", 
                     replace=FALSE,
                     caliper = .2, 
                     ratio = 1)
# see matchit function options here
# https://www.rdocumentation.org/packages/MatchIt/versions/1.0-1/topics/matchit
analytic.data$PS <- match.obj$distance
summary(match.obj$distance)
plot(match.obj, type = "jitter")
plot(match.obj, type = "hist")
tapply(analytic.data$PS, analytic.data$arthritis.type, summary)
# check how many matched
match.obj
# extract matched data
matched.data <- match.data(match.obj)


require(tableone)
baselinevars <- c("gender", "bmi", "diabetes", "smoke", "age", 
                  "race", "born", "education", "marriage", 
                  "annualincome", "physical.activity", "medical.access",
                  "blood.pressure", "healthy.diet", "covered.health")
tab1 <- CreateTableOne(strata = "arthritis.type", vars = baselinevars,
                       data = analytic.data, test = FALSE)
print(tab1, smd = TRUE)
tab1m <- CreateTableOne(strata = "arthritis.type", vars = baselinevars, 
                        data = matched.data, test = FALSE)
print(tab1m, smd = TRUE)




fit <- glm(I(heart.attack=="Yes") ~ arthritis.type, 
           family="binomial", data=matched.data)
require(jtools)
summ(fit, exp = TRUE)


# fit logistic regression
PS.fit2 <- glm(ps.formula,family="binomial", data=matched.data)
summary(PS.fit2)
