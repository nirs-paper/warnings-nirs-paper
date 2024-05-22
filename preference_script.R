# Библиотеки
library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(gtsummary)
library(remotes)
library(report)

# Загрузим данные
preferences <- read_csv("preference.csv")

####### ОСНОВНЫЕ РАСЧЕТЫ #######

# Удалим ответы тех респондентов, которые ответили на контрольный вопрос неверно
preferences_filter <- preferences %>% filter(Control == 4)
preferences_filter %>% select(Age, Sex, Preference, Control) %>% report() # Дескриптивная статистика 

# Расчитаем наблюдаемые частоты для каждого типа предупреждающих сообщений
table(preferences_filter$Preference)
observed <- c(37, 37, 55, 40)

# Двухсторонний бинаминальный тест для кадого типа сообщения
type1 <- binom.test(x = 37, n = 169, p =.25) # Тип №1 // p-value = 0.376 // non-significant
type2 <- binom.test(x = 37, n = 169, p =.25) # Тип №2 // p-value = 0.376 // non-significant
type3 <- binom.test(x = 55, n = 169, p =.25) # Тип №3 // p-value = 0.026 // significant
type4 <- binom.test(x = 40, n = 169, p =.25) # Тип №4 // p-value = 0.723 // non-significant

####### ДОПОЛНИТЕЛЬНЫЕ РАСЧЕТЫ #######

table(preferences_filter$Preference, preferences_filter$Sex)

# Тест на независимость - Preference vs. Sex
result_sex <- chisq.test(preferences_filter$Preference, preferences_filter$Sex, correct = FALSE)
result_sex %>% report() # p-value = 0.4145 => non-significant 

# Тест на независимость - Preference vs. Experience
result_experience <- chisq.test(preferences_filter$Preference, preferences_filter$Experience, correct = FALSE)
result_experience %>% report() # p-value = 0.1169 => non-significant 

# Тест на независимость - Preference vs. Necessity
result_necessity <- chisq.test(preferences_filter$Preference, preferences_filter$Necessity, correct = FALSE)
result_necessity %>% report() # p-value = 0.1318 => non-significant 
