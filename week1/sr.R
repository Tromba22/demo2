data <- "C:\Users\alitr\OneDrive\Bureau\fund2\New folder\Econometrics Methods and Applications\week 1"
?read.csv()
install.packages("readxl")
library(readxl)
install.packages("MASS")
library(MASS)
dt <-read.csv(file = "./Dataset1.csv", as.is = "!stringsAsFactors")
str(dt)
dt$Observation <- as.factor(dt$Observation)
str(dt)
ggplot(dt, aes(x= Price, y = Sales))+
  geom_point()+
  xlab("Price")+
  ylab("Sales")+
  geom_smooth(method = 'lm')
f1<-lm(Sales ~ Price, data = dt)
summary(f1)$r.squared
tidy(f1, conf.int = TRUE)
2/sd(resid(f1))
mean(resid(f1))
hist(resid(f1))
library(ggplot2)
ggplot(dt, aes(x = Sales)) + 
  geom_histogram(color = "blue", fill="blue", bins  = 15)+
  geom_density(alpha= .2)
p <- ggplot(dt) +
  geom_histogram(aes(x = Sales, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black")
p
dt1 <- read.csv(file = "./TrainExer11.csv")
str(dt1)
dt1$Observation <- as.factor(dt1$Observation)
?par
?hist
?plot
par(mfrow = c(1,2))
hist(dt1$Age)
hist(dt1$Expenditures)
ggplot(dt1, aes(x = Age, y = Expenditures)) +
  geom_point()
summary(dt1$Expenditures)
?summary
describe(dt1)
?which
str(dt1)
mean(dt1$Expenditures[dt1$Age < 40])
mean(dt1$Expenditures[dt1$Age > 40])
t <- lm(Expenditures ~ Age, data = dt1)
p <-  as.data.frame(50)
colnames(p) <- "Age"
predict(t, p, Arg = 'predtiction')
ex3 <- read.csv("./TrainExer13.csv", colClasses = c('numeric', 'factor', 'numeric'))
str(ex3)
f <- lm(Winning.time.men~Game, data = ex3)
library(broom)
tidy(f)
?tidy
summary(f)$r.squared
summary(f)
mean(ex3$Game); mean(ex3$Winning.time.men)
datey<-data.frame(
  years = c(2008, 2012, 2016),
  Game = c(16, 17, 18)
)
predict(f, newdata = datey)
ex5 <- read.csv("./TrainExer151.csv")
str(ex5)
