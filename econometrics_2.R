Sys.setlocale("LC_ALL","UTF-8")
setwd("~/Documents/workspace_r")

library(psych)
library(dplyr)
library(ggplot2)
library(GGally)

d <- cars
glimpse(d)
head(d)
tail(d)
describe(d)
tail(d, 10)
d2 <- mutate(d, speed = 1.67*speed, dist = 0.3*dist, ratio=dist/speed)

qplot(data = d2, dist)
qplot(data = d2, dist, xlab="Длина тормозного пути (м)",
      ylab="Кол-во машин",
      main="Данные 1920х годов")

qplot(data = d2, speed, dist)

model <- lm(data = d2, dist ~ speed)
#model
beta_hat <- coef(model)
#beta_hat
eps_hat <- residuals(model)
#eps_hat
y <- d2$dist
y_hat <- fitted(model)
RSS <- deviance(model)
#RSS
TSS <- sum ( (y - mean(y)) ^ 2 )
#TSS
ESS <- TSS - RSS
#ESS
R2 <- ESS / TSS
#R2
#  Above is the first way to calculate R2
#  Second way to calculate R2 is below
#  R - is a correlation between y and y_hat
#  R = cor(y, y_hat), thus
#  R2 = cor(y, y_hat)^2
x <- model.matrix(model)
#x - matrix of speed vectors and intercepts

# создадим новый набор данных для прогнозирования 
# тормозного пути от скорости движения автомобиля
# в реалиях 1920х годов
nd <- data.frame(speed = c(40,60))
# теперь рассчитаем значения прогноза на основании
# выведенной модели для этого набора
predict(model, nd)
# построим график
qplot(data = d2, speed, dist) + stat_smooth(method = "lm")
