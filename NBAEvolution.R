library(modelr)
library(tidyverse)
library(corrplot)
library(caret)
library(car)

df2010 <- NBAEvolution_2010
df2018 <- NBAEvolution_2018

cor_df2010 <- df2010 %>% select(ThreePOffense:OppFTARate)
str(cor_df2010)
correlations <- round(cor(cor_df2010),2)
correlations
corrplot(correlations,order="hclust")

cor_df2018 <- df2018 %>% select(ThreePOffense:OppFTARate)
str(cor_df2018)
correlations <- round(cor(cor_df2018),2)
correlations
corrplot(correlations,order="hclust")

highCorr <- findCorrelation(correlations,cutoff=.9,names=TRUE) #find highly correlated predictors
highCorr

#NBAMODEL2010

m1 <- lm(Wins~ThreePOffense+TwoPOffense+ThreePDefense+TwoPDefense+Location+TOVPct+OppTOVPct+FTARate+OppFTARate, data=df2010)
class(m1)
summary(m1) 

anova(m1)

df_model2010 <- df2010 %>%
  add_predictions(m1) 
View(df_model2010)

#NBAMODEL2018

m2 <- lm(Wins~ThreePOffense+TwoPOffense+ThreePDefense+TwoPDefense+Location+TOVPct+OREBPct+OppOREBPct+OppTOVPct, data=df2018)
class(m2)
summary(m2) 

anova(m2)

df_model2018 <- df2018 %>% 
  add_predictions(m2)
View(df_model2018)