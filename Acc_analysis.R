library(ggplot2)
library(dplyr)
library(emmeans)
library(lme4)
library(rstatix)
library(knitr)
library(reshape2)
library(multcomp)
library(lmerTest)
library(car)
library(olsrr)
library(usdm)
library(performance)
library(FactoMineR)
library(factoextra)
library(Rita)
library(effectsize)
library(car)


acc_df <- read.csv("S1_S40_Acc.csv")

acc_sub <- acc_df %>%
  group_by(phonetic_cons,semantic_trans,Subject,PhonN)%>%
  summarise(acc_rate = 100-(sum(is.na(correctRT))/40*100)) 


acc_item <-  acc_df %>%
  group_by(phonetic_cons,semantic_trans,Stim,PhonN)%>%
  summarise(acc_rate = 100-(sum(is.na(correctRT))/40*100))

acc_sub$Subject <- as.factor(acc_sub$Subject)
acc_sub$semantic_trans <- as.factor(acc_sub$semantic_trans)
acc_sub$phonetic_cons <- as.factor(acc_sub$phonetic_cons)
acc_item$Stim <- as.factor(acc_item$Stim)
acc_item$semantic_trans <- as.factor(acc_item$semantic_trans)
acc_item$phonetic_cons <- as.factor(acc_item$phonetic_cons)


#elogit transformation
#y = accuracy rate
#n = 40
elogit_manual <- function (vec,n){
  new_vec <- c()
  for (i in vec){
    temp <- log((i+0.5*n)/(1-i+0.5*n))
    new_vec <- append(new_vec, temp)
  }
  return(new_vec)
}


test3 <- (acc_sub$acc_rate)/100
test3 <- data.frame(elogit_manual(test3,40))
colnames(test3)[1] <- "elogit_acc"
acc_sub_new_2 <- cbind(test3, acc_sub)

test4 <- (acc_item$acc_rate)/100
test4 <- data.frame(elogit_manual(test4,40))
colnames(test4)[1] <- "elogit_acc"
acc_item_new_2 <- cbind(test4, acc_item)



#ANOVA/ANCOVA analysis
two.way.acc.sub <- aov(elogit_acc~ phonetic_cons*semantic_trans+PhonN +Error(Subject/(phonetic_cons*semantic_trans+PhonN)), data = acc_sub_new_2)
summary(two.way.acc.sub)
emmeans(two.way.acc.sub, ~PhonN)

two.way.acc.it <- aov(elogit_acc~ phonetic_cons*semantic_trans , data = acc_item_new_2)#+PhonN
summary(two.way.acc.it)
emmeans(two.way.acc.it, pairwise~phonetic_cons |semantic_trans)



####calculate effect size
effectsize::eta_squared(two.way.acc.sub, partial = FALSE)
effectsize::eta_squared(Anova(two.way.acc.it,type = 2), partial = FALSE)
