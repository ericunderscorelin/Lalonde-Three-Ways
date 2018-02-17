setwd("~/Documents/Hyderabad/CS112/Lalonde Three Ways")
library(Matching)
library(randomForest)
library(arm)
data("lalonde")

#adding new column
lalonde$u78 <- 0
lalonde$u78[lalonde$re78 == 0] <- 1

#randomforresting to choose predictors
set.seed(2222)
superdopeforrest <- randomForest(re78~.-u78, data=lalonde
                                 ,mtry=4, ntree = 5000, 
                                 importance =TRUE)

superdopeforrest
importance(superdopeforrest)
varImpPlot(superdopeforrest)

#creating degree/nodegree dataframes from lalonde
no_degree_group <- lalonde[which(lalonde$nodegr ==1),]
with_degree_group <- lalonde[which(lalonde$nodegr ==0),]

#linear regression 
no_dgr_regression <- lm(re78 ~ treat + re74 + re75, 
                        data = no_degree_group)

with_dgr_regression <- lm(re78 ~ treat + re74 + re75, 
                      data = with_degree_group)
no_dgr_regression
with_dgr_regression

summary(no_dgr_regression)
summary(with_dgr_regression)

confint (no_dgr_regression)
confint (with_dgr_regression)

#linear regression with interaction term
int_term <- lm(re78 ~ treat + nodegr + I(treat*nodegr), 
                          data = lalonde) #assuming that nodegr has some effect on treatment
int_term
display(int_term)

int_term_sim <- sim(int_term)
mean_treat_coef <- mean(coef(int_term_sim)[,2])
mean_treat_coef
mean_inter_coef <- mean(coef(int_term_sim)[,4])
mean_inter_coef
#treatment effect = coef(interaction.term)*nodegr + coef(treatment)
treat_effect_ndeg = mean_inter_coef*1 + mean_treat_coef
treat_effect_ndeg 
treat_effect_wdeg = mean_inter_coef*0 + mean_treat_coef
treat_effect_wdeg 


summary(int_term) 
confint (int_term)



#Datavizing log reg treatment effects
lalonde$nodegr <- factor(lalonde$nodegr)
lalonde$treat <- factor(lalonde$treat)
library(ggplot2)
ggplot(lalonde, aes(x = nodegr, y = re78, fill=treat))+geom_boxplot()


#logistic regression
#rerun to update the split dataframes 
#since lalonde has been changed
no_degree_group <- lalonde[which(lalonde$nodegr ==1),]
with_degree_group <- lalonde[which(lalonde$nodegr ==0),]

logreg_ndeg <- glm(u78 ~ treat + re74 + re75, 
              data = no_degree_group, family = binomial)
logreg_wdeg <- glm(u78 ~ treat + re74 + re75, 
                   data = with_degree_group, family = binomial)

summary(logreg_ndeg)
summary(logreg_wdeg)

confint (logreg_ndeg)
confint (logreg_wdeg)

#treatment effect without degree
predict_ndeg <- predict(logreg_ndeg, no_degree_group[1,], type = 'response')
predict_ndeg
no_degree_group[1,]$treat
no_degree_group[1,]$treat <- 0
no_treat_ndeg <- predict(logreg_ndeg,no_degree_group[1,], type = 'response')
no_treat_ndeg
predict_ndeg-no_treat_ndeg #probability of being unemployed is decreased by 9.7%
no_degree_group[1,]$treat <- 1 #changing it back

#treatment effect with degree
predict_wdeg <- predict(logreg_wdeg, with_degree_group[1,], type = 'response')
predict_wdeg
with_degree_group[1,]$treat
with_degree_group[1,]$treat <- 0
no_treat_wdeg <- predict(logreg_wdeg, with_degree_group[1,], type = 'response')
no_treat_wdeg
predict_wdeg-no_treat_wdeg #probability of being unemployed is decreased by 16.2%
with_degree_group[1,]$treat <- 1 #changing it back
