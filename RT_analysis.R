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


##load data
data <- read.csv("S1_S40_RT.csv") #RT and other variables for ancova/anova

##RT subject analysis (PhonN has been removed due to insignificance)
mean_RT_sub <- aggregate(data$correctRT, list(data$Subject), FUN = mean) 
sd_RT_sub <- aggregate(data$correctRT, list(data$Subject), FUN = sd) 
colnames(mean_RT_sub) <- c("Subject", "mean_RT") 
colnames(sd_RT_sub) <- c("Subject","sd_RT") 
df_sub <- cbind(mean_RT_sub, sd_RT = sd_RT_sub$sd_RT)
df_sub$remove_higher_sub <- df_sub$mean_RT+3*df_sub$sd_RT
df_sub$remove_lower_sub <- df_sub$mean_RT-3*df_sub$sd_RT

num <- 1:40
new_data_sub <- data.frame()
for (p in num){
  temp3 <- filter(data, Subject == df_sub$Subject[p])
  temp3 <- subset(temp3, df_sub$remove_lower_sub[p]< correctRT & correctRT < df_sub$remove_higher_sub[p])
  new_data_sub <- rbind(new_data_sub, temp3)
}

data_mean_sub <- new_data_sub %>%
  group_by(phonetic_cons,semantic_trans,Subject)%>%
  summarise(mean = mean(correctRT)) #for subject analysis

data_mean_sub$Subject <- as.factor(data_mean_sub$Subject)
data_mean_sub$semantic_trans <- as.factor(data_mean_sub$semantic_trans)
data_mean_sub$phonetic_cons <- as.factor(data_mean_sub$phonetic_cons)

rm_aov2 <- aov(mean~ phonetic_cons*semantic_trans + Error(Subject/(phonetic_cons*semantic_trans)) , data = data_mean_sub) #by-subject
summary(rm_aov2)
emmeans(rm_aov2, pairwise~semantic_trans|phonetic_cons)


## RT item analysis
mean_RT_item <- aggregate(data$correctRT, list(data$Stim), FUN = mean)
sd_RT_item <- aggregate(data$correctRT, list(data$Stim), FUN = sd)
df_item <- cbind(mean_RT_item, sd_RT_item$x)
colnames(df_item) <- c("Item", "mean", "sd")
df_item$remove_higher_item <- df_item$mean+3*df_item$sd
df_item$remove_lower_item <- df_item$mean-3*df_item$sd

new_data_item <- data.frame()
for (k in num){
  temp2 <- filter(data, Stim == df_item$Item[k])
  temp2 <- subset(temp2, df_item$remove_lower_item[k]< correctRT & correctRT < df_item$remove_higher_item[k])
  new_data_item <- rbind(new_data_item, temp2)
}

data_mean_item <- new_data_item %>%  
  group_by(phonetic_cons,semantic_trans,Stim,PhonN)%>%
  summarise(mean = mean(correctRT)) #for item analysis

rm_aov3 <- aov(mean~ phonetic_cons*semantic_trans + PhonN, data = data_mean_item) #by-item
summary(rm_aov3)
emmeans(rm_aov3, pairwise~semantic_trans|phonetic_cons)


