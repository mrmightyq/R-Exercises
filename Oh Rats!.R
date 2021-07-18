library(readr)
library(ggpubr)
library(dplyr)
library(car)
library(multcomp)
rats <- read_csv("C:/Users/KnudseQ/Desktop/R data/rats.csv")
rats$Day <- as.factor(rats$Day)
rats$TreatmentRat <- as.factor(rats$TreatmentRat)
rats$Treatment <- as.factor(rats$Treatment)
str(rats)
summary(rats)
#Outcome:
#Factors: Weight, Treatment, Day 

#Test for Homogeneity of variances
leveneTest(Weight ~ Treatment*Day, data = rats)

rats_weight_scale <- scale(rats$Weight)
#Let's take a look at some plots
ggboxplot(rats, x = "Day", y = "rats_weight_scale", color = "Treatment",
          palette = c("#00AFBB", "royalblue3","springgreen3","#E7B800","red2"))


ggline(rats, x = "Day", y = "Weight", color = "Treatment",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "royalblue3","springgreen3","#E7B800","red2"))

#Replicate the mean plot
x<- aggregate(rats, list(rats$Treatment,rats$Day), mean,na.rm = TRUE)
rats2 <- x[1:3]
rats2$Day <- rats2$Group.2
rats2$Treatment <- rats2$Group.1
ggline(rats2, x = "Day", y = "Weight", color = "Treatment",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "royalblue3","springgreen3","#E7B800","red2"))



#Now the ANOVA
res.aov <- aov(Weight ~ Treatment + Day, data = rats)
summary(res.aov)
TukeyHSD(res.aov, which = c("Treatment","Day"))
summary(glht(res.aov, linfct = mcp(Treatment = "Tukey")))

#Let's group these together 
model.tables(res.aov, type="means", se = TRUE)

rats %>%
  group_by(Treatment,Day) %>%
  summarise_at(vars(-TreatmentRat), funs(mean(., na.rm=TRUE)))


rats_day0 <- rats[rats$Day=="0",]
res.aov <- aov(Weight ~ Treatment, data = rats_day0)
summary(res.aov)
TukeyHSD(res.aov, which = c("Treatment","Day"))
summary(glht(res.aov, linfct = mcp(Treatment = "Tukey")))

t.test(rats[rats$Treatment=="T21",1],
       rats[rats$Treatment=="T27",1])

t.test(rats_day0[rats_day0$Treatment=="T21",1],
       rats_day0[rats_day0$Treatment=="T27",1])


library(ez)
y<-ezANOVA(data=rm1.complete,
        wid=Treatment_Combined,
       within=.(Treatment,Day),
       dv = Weight,
       detailed=T,
       type = 3)

rm1.complete <- rats2[complete.cases(rats2),]
anova_lme_loc<-lme(data=rats2, fixed=Weight ~ Treatment * Day, random=~1|Treatment_Combined,
                   na.action=na.omit)
anova(anova_lme_loc)
summary(glht(anova_lme_loc, linfct=mcp(Day = "Tukey")), test = adjusted(type = "bonferroni"))




rats2 <- read_csv("C:/Users/KnudseQ/Desktop/R Data/Rats2.csv")
rats2$Day <- as.factor(rats2$Day)
rats2$Number <- as.factor(rats2$Number)
rats2$Treatment <- as.factor(rats2$Treatment)
rats2$Treatment_Combined <- as.factor(rats2$Treatment_Combined)
rats2$ID <- as.factor(rats2$ID)
rats2<- na.omit(rats2)
summary(rats2)
library(rstatix)
rats2 <- na.exclude(rats2)
res.aov2 <- anova_test(
  data = rats2, dv = Weight, wid = ID,
  within = c(Treatment, Day)
)
get_anova_table(res.aov2)

rats2$Group <- rats2$Treatment
x <-lm(Weight~ID+Treatment+Day,data=rats2)
summary(x)


#2 Way Repeated Measures ANOVA
library(nlme)
model = lme(Weight ~ Treatment * Day, data = rats2, random = ~1|ID)
anova(model)

library(emmeans)
emmeans(model, pairwise ~ Treatment * Day)

emm <- emmeans(model, ~ Treatment * Day)
pairs(emm, simple = "each")


#Now the ANOVA
res.aov <- aov(Weight ~ Treatment + Day, data = rats2)
summary(res.aov)
TukeyHSD(res.aov, which = c("Treatment","Day"))
summary(glht(res.aov, linfct = mcp(Treatment = "Tukey")))



res.aov <- anova_test(data=rats, dv = Weight, wid=Treatment, within=Day)
get_anova_table(res.aov)


ggboxplot(rats, x = "Treatment", y = "Weight")
res.aov <- rats %>% anova_test(Weight ~ Treatment)
res.aov
pwc <- rats %>% tukey_hsd(Weight ~ Treatment)
pwc
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Treatment")
ggboxplot(rats, x = "Treatment", y = "Weight") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


ggboxplot(rats, x = "Day", y = "Weight")
res.aov <- rats %>% anova_test(Weight ~ Day)
res.aov
pwc <- rats %>% tukey_hsd(Weight ~ Day)
pwc
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Day")
ggboxplot(rats, x = "Day", y = "Weight") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
