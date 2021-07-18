# if needed, get job_retention data
url <- "http://peopleanalytics-regression-book.org/data/job_retention.csv"
job_retention <- read.csv(url)
head(job_retention)


library(survival)

# create survival object with event as 'left' and time as 'month'
retention <- Surv(event = job_retention$left, 
                  time = job_retention$month)

# view unique values of retention
unique(retention)
# kaplan-meier estimates of survival by gender
kmestimate_gender <- survival::survfit(
  formula = Surv(event = left, time = month) ~ gender, 
  data = job_retention
)

summary(kmestimate_gender)

# create a new field to define high sentiment (>= 7)
job_retention$sentiment_category <- ifelse(
  job_retention$sentiment >= 7, 
  "High", 
  "Not High"
)

# generate survival rates by sentiment category
kmestimate_sentimentcat <- survival::survfit(
  formula = Surv(event = left, time = month) ~ sentiment_category,
  data = job_retention
)

summary(kmestimate_sentimentcat)



library(survminer)

# show survival curves with p-value estimate and confidence intervals
survminer::ggsurvplot(
  kmestimate_sentimentcat,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("blue", "red"),
  linetype = c("solid", "dashed"),
  xlab = "Month",
  ylab = "Retention Rate"
)


# run cox model against survival outcome
cox_model <- survival::coxph(
  formula = Surv(event = left, time = month) ~ gender + 
    field + level + sentiment,
  data = job_retention
)

summary(cox_model)

(ph_check <- survival::cox.zph(cox_model))
survminer::ggcoxzph(ph_check, 
                    font.main = 10, 
                    font.x = 10, 
                    font.y = 10)

library(frailtypack)

(frailty_model <- frailtypack::frailtyPenal(
  formula = Surv(event = left, time = month) ~ gender + 
    level + sentiment + cluster(field),
  data = job_retention,
  n.knots = 12, 
  kappa = 10000
))


stratified_base <- frailtypack::frailtyPenal(
  formula = Surv(event = left, time = month) ~ 
    strata(sentiment_category),
  data = job_retention,
  n.knots = 12,
  kappa = rep(10000, 2)
)

plot(stratified_base, type.plot = "Survival", 
     pos.legend = "topright", Xlab = "Month",
     Ylab = "Baseline retention rate",
     color = 1)
