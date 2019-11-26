library(tidyverse)
library(readr)

nlsaa<-read_csv("NLSAA.csv")

#test normal distribution by shapiro test####
shapiro.test(nlsaa$lsi_85)

# w is the statiscal value
# p value <0.05, then we can reject the null hypothesis

#try asleep in 1993
shapiro.test(nlsaa$tmasl_93)
#in this test, the p-value > 0.05
#it cannot reject the Null hypothesis

# Q-Q test to check how far from the normal distribution
qqnorm(nlsaa$lsi01_85)
qqline(nlsaa$lsi01_85)

qqnorm(nlsaa$tmasl_93)
qqline(nlsaa$tmasl_93)

####student t-test which assume the data follow the normal distribution####

#fuction: t-test()
#three types of tests
#1. one sample
#2. two sample unpaired(no link between data points)
#3. two sample paired

#Null hypothesis is that mean = 28
t.test(nlsaa$rgrp4_93, mu = 28) 

#Null hypothesis is that 89_mean = 93_mean
t.test(nlsaa$ivdur_89, nlsaa$ivdur_93)


#using parameter paired = TRUE
#paired parameter is to specify that a group people in different year
t.test(nlsaa$ivdur_89, nlsaa$ivdur_93, paired = TRUE)

#check the differenct between male and female
#Null hypothesis: there's no difference
t.test(nlsaa$ivdur_89 ~ nlsaa$sex_89) #note using "~"

####Mann-Whitney U test(MWW) ####
#A version of t-test that doesn't assume 
#the data follow normal distribution
#wilcox.test()
wilcox.test(nlsaa$ivdur_89, nlsaa$ivdur_93)


####Analysis of Variance = ANOVA ####
#extension of t-test because of more than 2 groups 
#null hypothesis :
#the different groups are sample of the same population
  #if reject the Null hypothese 
  # means that there are huge difference among groups

anova <- aov(nlsaa$wght_85 ~ factor(nlsaa$agesex85))
summary(anova)

#use the plots to visualise the relationships
#box plot

ggplot(nlsaa, aes(x = factor(agesex85), y = wght_85)) +
  geom_boxplot()

##Tukey trst to see if the trend in significant####
TukeyHSD(anova)

##Kruska-wallis test####
#Null hypothesis: all the groups are the same; 
#Null hypothesis: and are sample from the same distribution
#extend Mann-Whiteney U test to multiple groups.
#non_parametric
kruskal.test(nlsaa$wght_85 ~ factor(nlsaa$agesex85))
#the p-value < 0.05, we can reject the null hypothesis
#there are difference among the groups.

#pairwise.wilcox to paired comparison the kruskal

pairwise.wilcox.test(nlsaa$wght_85, factor(nlsaa$agesex85),
                     p.adjust.method = "BH")

#chisq.test####
#Null hypothesis: the frequency of one variable 
#do not change based on the categories of a second variable

chisq.test(nlsaa$dep_c_89, nlsaa$qfall_89)

#chechk the distribution using prop.table()
table(nlsaa$dep_c_85, nlsaa$qfall_89)

prop.table(table(nlsaa$dep_c_85, nlsaa$qfall_89), 1)



