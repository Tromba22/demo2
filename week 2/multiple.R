library(ggplot2)
library(broom)
library(tidyverse)
library(lubridate)
install.packages("tinytex")
library(tinytex)
tinytex::install_tinytex()
ex1 <- read.csv(file = "./TrainExer21.csv")
str(ex1)
fe <- ex1 %>%
  filter(Female == 1)
fe
fun <- lm(LogWage ~ Female, data = ex1)$coef
fun
fun[1]*10
summary(fun)
errors <- function(dat){
  co <- lm(LogWage ~ Female, data = dat)$coef
  e <- dat$LogWage - co[1] - co[2]* dat$Female
  return(e)
}
ex1i<-ex1 %>%
  mutate(error = errors(.))
head(ex1i)
co <- ex1 %>%
  lm(log(Wage) ~ Female, data = .) %>%
  .$coef
fi <- lm(error ~ Educ, data = ex1i)
summary(fi)
fii <- lm(error ~ Parttime, data = ex1i)
summary(fii)
