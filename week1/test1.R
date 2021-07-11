library(ggplot2)
library(broom)
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
extest <- read.csv(file = "./TestExer1-holiday expenditures-round2.csv", 
                   colClasses = c('factor', 'numeric', 'numeric'))
str(extest)
mean(extest$Expenditures);mean(extest$Age)
fu <- lm(Expenditures ~ Age, data = extest)
tidy(fu)
?glance
?t.test
t.test(fu)
summary(fu)
ggplot(extest, aes(x= Age, y = Expenditures))+
  geom_point()+
  xlab("Age")+
  ylab("Expenditures")+
  geom_smooth(method = 'lm')
below <- extest %>% filter(Age < 40)
above <- extest %>% filter(Age >= 40)
fu2 <- lm(Expenditures ~ Age, data = below)
fu3 <- lm(Expenditures ~ Age, data = above)
summary(fu2)
summary(fu3)
