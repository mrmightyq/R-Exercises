#Load Data
df = read.csv(file = 'C:/Users/KnudseQ/Desktop/R Desktop/HoF/nba_groomed_data.csv', header = TRUE, encoding = 'UTF-8', na.strings=c('', 'NA'))
#remove columns with no statistical value
df_stats = df[, !(colnames(df) %in% c("X","bling", "name_url", "team_url", "team", "college"))]
#set NA = 0
df_stats[is.na(df_stats)] = 0

#split to HOF eligible players only as of 2019
df_hof = df_stats[(df_stats$draft_year + df_stats$career_years < 2015) & (df_stats$games_played > 0),]
df_future = df_stats[(df_stats$draft_year + df_stats$career_years >= 2015) & (df_stats$games_played > 0),]

removals = c('draft_year', 'pick', 'name')
full_model = glm(hall_of_fame ~ ., data = df_hof[, !(colnames(df_hof) %in% removals)], family = binomial)
summary(full_model)

step_model = step(full_model, direction = 'both', trace = FALSE)

anova(step_model, full_model, test = 'LRT')

summary(step_model)

modified_full_model = glm(hall_of_fame ~ . - scoring_champ - mvp, data = df_hof[, !(colnames(df_hof) %in% removals)], family = binomial)
modified_step_model = step(modified_full_model, direction = 'both', trace = FALSE)
summary(modified_step_model)

new_model = glm(hall_of_fame ~ minutes_played + total_points + 
                  career_fgp + career_ws + all_star + all_rookie + 
                  as_mvp + all_nba + nba_champ + all_defensive, family = binomial, 
                data = df_hof[, !(colnames(df_hof) %in% removals)])

anova(new_model, modified_step_model, test = 'LRT')

library(boot)

set.seed(1)
cv.glm(df_hof[, !(colnames(df_hof) %in% removals)], new_model, K = 10)$delta[1]

set.seed(1)
cv.glm(df_hof[, !(colnames(df_hof) %in% removals)], modified_step_model, K = 10)$delta[1]

summary(new_model)

library(boot)

set.seed(1)
cv.glm(df_hof[, !(colnames(df_hof) %in% removals)], modified_step_model, K = 10)$delta[1]

set.seed(1)
cv.glm(df_hof[, !(colnames(df_hof) %in% removals)], new_model, K = 10)$delta[1]

df_hof$hof_predict = predict(new_model, newdata = df_hof, type = "response")

hist(df_hof[df_hof$hall_of_fame == 1,]$hof_predict,
     breaks = 10,
     main="Hall of Fame Probability Histogram",
     xlab = "Hall of Fame Probability",
     col = "firebrick1"
)

plot(df_hof$hof_predict, jitter(df_hof$hall_of_fame, .1),
     main = "Hall of Fame Status vs Probability",
     xlab = "Hall of Fame Probability",
     ylab = "Hall of Fame Status (1 = in HoF)")

#sort dataframe by probability
df_hof = df_hof[order(-df_hof$hof_predict),]
df_hof[df_hof$hof_predict >= .5 & df_hof$hall_of_fame == 0, c("name", "hof_predict")]


df_hof[df_hof$hof_predict <= .2 & df_hof$hall_of_fame == 1, c("name", "hof_predict")]

#sort dataframe by probability
df_future$hof_predict = predict(new_model, newdata = df_future, type = "response")
df_future = df_future[order(-df_future$hof_predict),]

future_HoF <- df_future[df_future$hof_predict > .5, c('name', 'hof_predict')]

library(ggplot2)
library(dplyr)
library(forcats)

future_HoF %>%
  mutate(name = fct_reorder(name, hof_predict)) %>%
  ggplot( aes(x=name, y=hof_predict)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") + ylab("Hall of Fame Probability")+ ggtitle("Hall of Fame Analysis")+
  theme_bw()



#Exterme Gradient Boost
library(gbm)
library(caret)
library(randomForest)
levels(df_hof$hall_of_fame) <- c("NotHallofFame", "HallofFame")
df_hof$hall_of_fame<-as.factor(df_hof$hall_of_fame)
df_hof$name <- as.character(df_hof$name)
formula = hall_of_fame~.- name
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 12,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)
XGB.model <- train(formula, data = df_hof,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)
ggplot(varImp(XGB.model)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()
library(pROC)
XGB.prd <- predict(XGB.model,df_hof)
confusionMatrix(XGB.prd, df_hof$hall_of_fame)
XGB.plot <- plot.roc (as.numeric(df_hof$hall_of_fame),
                      as.numeric(XGB.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
XGB.plot

# Partioning the data train 80% test 20%
set.seed(1234)
ind <- sample(2, nrow(df_hof), replace =T, prob = c(0.8, 0.2))
train <- df_hof[ind==1,]
test <- df_hof[ind==2,]


#Random Forest 
rf <- randomForest(as.factor(hall_of_fame)~.-name, data = train)
print(rf)
#Prediction & Confusion Matrix - train data 
rf.prd <- predict(rf, test)
confusionMatrix(rf.prd, test$hall_of_fame)


#plot ROC
rf.plot<- plot.roc(as.numeric(test$hall_of_fame), as.numeric(rf.prd),lwd=2, type="b",print.auc=TRUE,col ="blue")


library(neuralnet)
library(NeuralNetTools)
library(nnet)
Net2 <-neuralnet(as.numeric(hall_of_fame) ~ .-name, df_hof, hidden=c(4,2),lifesign = "minimal", linear.output = FALSE, threshold = 0.1)
plot(Net2)


NN.plot <- plot.roc (as.numeric(df_hof$hall_of_fame),
                      as.numeric(NN.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
