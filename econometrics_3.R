Sys.setlocale("LC_ALL","UTF-8")
setwd("~/Documents/workspace_r")

library(psych)
library(dplyr)
library(ggplot2)
library(GGally)

t <- swiss
glimpse(t)
describe(t)
ggpairs(t)

# построим модель зависимости фертильности от
# "сельской местности", уровня образования и 
# вероисповедания (католицизм)
model2 <- lm(data = t,
             Fertility~Agriculture+Education+Catholic)
# оценим полученные моделью коэффициенты
# (видно, что доля сельского населения отрицательно
# сказывается на фертильности, как и уровень
# образования, зато вероисповедание положительно
# влияет на уровень фертильности)
coef(model2)

fitted(model2) # спрогнозированные значения фертильности
residuals(model2) # epsilon_hat
deviance(model2) # RSS

# рассчитаем R2 еще одним способом
report <- summary(model2)
R2 <- report$r.squared
#R2
#  R2 = cor(t$Fertility, fitted(model2))^2

nd2 <- data.frame(Agriculture = 0.5,
                  Catholic = 0.5,
                  Education = 20)
predict(model2, nd2)
