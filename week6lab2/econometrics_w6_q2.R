# Set Locale to UTF-8
Sys.setlocale("LC_ALL", "UTF-8")

# Load library RLMS
library(rlms)
# Load library dplyr
library(dplyr)
# Load library tidyr
library(tidyr)
# Load library ggplot2
library(ggplot2)
# Load library lmtest
library(lmtest)
# Load library sandwich
library(sandwich)

# Set working Directory and data filename
setwd("~/Education/Practice/R")
datafile <- "r22i_os25e.sav"

# Check if data file exists, and if not, then download it
if(!file.exists(datafile)) {
  file <- "https://www.hse.ru/data/2016/10/29/1313524401/r22i_os25e.sav"
  download.file(file, destfile = datafile, method = "curl")
}

# Load data from file into R
df <- rlms_read(datafile)

# Do vars cleanup
#rm("file")
rm("datafile")

# Do test Data.Frame
# head(df)
# tail(df)
# glimpse(df)

# Select and rename only
# - rj13.2 (salary)
# - rh6 (birth year)
# - rh5 (sex)
# - r_diplom (education)
# - status (location status, i.e. city, village, etc)
# - rj1.1.1 (job satisfaction level)
# Results in 16087 observations with 6 variables
seldf <- select(df, salary=rj13.2, age=rh6, sex=rh5, education=r_diplom, location = status, jobsatisfaction=rj1.1.1)

# Mutate Age to reflect true age rather than year of birth
# Since poll was from 2013, age = (2013 - age) in years
seldf <- mutate(seldf, age1 = 2013 - age)
seldf <- select(seldf, -age)
seldf <- rename(seldf, age = age1)

# Select based on location status
# - city
# - regional center
# Results in 10692 observations with 6 variables
seldf <- filter(seldf, location=='город' | location=='областной центр')

# Select based on job satisfaction
# - completely satisfied
# - rather satisfied
# Results in 3547 observations with 6 variables
seldf <- filter(seldf, jobsatisfaction=='ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ' | jobsatisfaction =='СКОРЕЕ УДОВЛЕТВОРЕНЫ')

# Select based on education (exclude following)
# - no answer
# - decline to answer
# - hard to answer
# Results in 3540 observations with 6 variables
seldf <- filter(seldf, education!='ЗАТРУДНЯЮСЬ ОТВЕТИТЬ' | education!='ОТКАЗ ОТ ОТВЕТА' | education!='НЕТ ОТВЕТА')

# Make dummy variable city set to either 0 or 1
seldf <- mutate(seldf, city=0)
seldf$city <- factor(ifelse(seldf$location=='город', 1, 0), labels=c(0, 1))
seldf <- select(seldf, -location)

# Make dummy variable job set to either 0 or 1
# - 0 for rather satisfied
# - 1 for completely satisfied
seldf <- mutate(seldf, job=0)
seldf$job <- factor(ifelse(seldf$jobsatisfaction=='ПОЛНОСТЬЮ УДОВЛЕТВОРЕНЫ', 1, 0), labels=c(0, 1))
seldf <- select(seldf, -jobsatisfaction)

# Make dummy variable sex set to either 0 or 1
# - 0 for women
# - 1 for men
seldf$sex <- factor(ifelse(seldf$sex=='МУЖСКОЙ', 1, 0), labels=c(0, 1))

# Make dummy variables for education:
# - 0 for unfinished education
# - 1 for finished mid level
# - 2 for finished special
# - 3 for finished higher level
seldf <- mutate(seldf, edu0=0)
seldf$edu0 <- factor(ifelse((seldf$education=='окончил 0 - 6 классов'
              | seldf$education=='незаконченное среднее образование (7 - 8 кл)'
              | seldf$education=='незаконченное среднее образование (7 - 8 кл) + что-то еще'),
              1, 0), labels = c(0, 1))

seldf <- mutate(seldf, edu1=0)
seldf$edu1 <- factor(ifelse(seldf$education=='законченное среднее образование',
              1, 0), labels=c(0,1))

seldf <- mutate(seldf, edu2=0)
seldf$edu2 <- factor(ifelse(seldf$education=='законченное среднее специальное образование',
              1, 0), labels=c(0,1))

seldf <- mutate(seldf, edu3=0)
seldf$edu3 <- factor(ifelse(seldf$education=='законченное высшее образование и выше',
              1, 0), labels=c(0,1))

seldf <- select(seldf, -education)

# Remove NAs
# Results in  observations with 6 variables
seldfnona <- drop_na(seldf)

save.image(file="week6lab2.RData")

# Question 1 - what is the max age in DF prior to NA cleanup
# 87
max(seldf$age)

# Question 2 - how many NAs there are prior to cleanup
# 557
sum(is.na(seldf$salary))

# Question 3 - using histogram, is absolute majority
# having salary below 50K RUR?
# Yes
hist(seldfnona$salary)

# Question 4 - two plots
# on first - Salary distribution by sex
qplot(data = seldfnona,
      x = (salary/1000),
      ylab = 'Частота',
      facets = ~sex,
      binwidth = 10)

# on second - Age distribution by sex
qplot(data = seldfnona,
      x = age,
      ylab = 'Частота',
      facets = ~sex,
      binwidth = 10)

# Question 5 - linear regression model
# salary ~ age + sex + edu1 + edu2 + edu3 + city + job
# F-statistic: 59.11 on 7 and 2975 DF
model <- lm(salary ~ age + sex + edu1 + edu2 + edu3 + city + job, data = seldfnona)
summary(model)

# Question 6 - for model is Ho (all vars = 0, thus unimportant)
# Ho shall be rejected, at least some of the vars are important

# Question 7 - which t-stat shall one use for sex var
# 14.9546
coeftest(model)

# Question 8 - men's salary is higher

# Question 9 - consistent se for dummy var city
# NO!!!! 882.062
#coeftest(model, vcov. = vcovHAC(model))
# 602.813
coeftest(model, vcov. = vcovHC(model))

# Question 10 - more satisfied people get higher rate by
# 1314.5 RUR