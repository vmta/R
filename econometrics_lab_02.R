# if you see KRAKOZYABRY then do 
# "File" - "Reopen with encoding" - "UTF-8" - (Set as default) - OK

# lab 2

# загружаем пакеты
library("memisc") # две и более регрессий в одной табличке
library("dplyr") # манипуляции с данными
library("psych") # описательные статистики
library("lmtest") # тестирование гипотез в линейных моделях
library("sjPlot") # графики
library("sgof")
library("ggplot2") # графики
library("foreign") # загрузка данных в разных форматах
library("car")
library("hexbin") # графики
library("rlms") # загрузка данных в формате rlms (spss)

# генерируем случайные величины
# z_1. ... z_100 - N(5, 9)
z <- rnorm(100, mean=5, sd=3)

# построим график плотности
x <- seq(-8, 15, by = 0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x,y,geom="line")

# P(z < 3)
# P(z < 3) = F(z)
pnorm(3, mean=5, sd=3)

# P(z in [4:9])
# P(z < 9) - P(z > 4)
pnorm(9, mean=5, sd=3) - pnorm(4, mean=5, sd=3)

# P(z < a) = 0.7; a?
qnorm(0.7, mean=5, sd=3)



# Множественная регрессия, проверка гипотез
h <- swiss
glimpse(h)

model <- lm(data = h, Fertility~Catholic+Agriculture+Examination)
summary(model)
coeftest(model)
confint(model)
sjp.lm(model)

# Проверка гипотезы b_Cath = b_Agri
model_aux <- lm(data = h,
                Fertility~Catholic+
                  I(Catholic+Agriculture)+
                  Examination)
summary(model_aux)
# linearHypotesis(model, "Catholic-Agriculture=0")
# Note: library will not install...
# p-val => 0.158483, т.е. нулевая гипотеза не отвергается


# стандартизированные переменные
# 
# шаг 1
h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data = h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
sjp.lm(model_st)
sjp.lm(model, showStandardBeta = TRUE)


# искусственный эксперимент
D <- matrix(nrow = 100, rnorm(100*41, mean = 0, sd = 1))
df <- data.frame(D)
model_pusto <- lm(data = df, X1~.)
summary(model_pusto)


# сравнить несколько моделей
model2 <- lm(data = h, Fertility~Catholic+Agriculture)
#compare_12 <- mtable(model, model2)
# note - library memisc not installed



# сохранение результатов работы в формате R - RDS
stuff <- list(data=h, model2)
saveRDS(file = "mytestdata.RDS", stuff)
mylist <- readRDS("mytestdata.RDS")


# сохранение результатов работы в формате CSV
# t <- read.csv("filename.csv", sep = "\t", dec = ".", header = TRUE)

# Загрузка в формате RLMS
h_ <- rlms_read("../Education/Data-Science/HSE/Data_econometrics_pre.sav")