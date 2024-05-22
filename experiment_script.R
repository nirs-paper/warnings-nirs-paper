# Библиотеки
library(car)
library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(report)

# Загрузим данные
experiment <- read_csv("Experiment1.csv")
experiment %>% select(Age, Sex, Education) %>% report() # Дескриптивная статистика 

####### ОСНОВНЫЕ РАСЧЕТЫ #######

# Удалим ответы тех респондентов, которые ответили на контрольный вопрос неверно
experiment_filter <- experiment %>% filter(Control == "3-4")
experiment_filter %>% select(Age, Sex, Education) %>% report() # Дескриптивная статистика 

# Проверим сбалансированность экспериментальных групп
table(experiment_filter$Type)

#### Статистические предположения #### 

# Гомогенность дисперсий - Лайки / Репосты / Комментарии
var_test_likes = leveneTest(Likes ~ Type, experiment_filter) # p-value = 0.6869 => дисперсии примерно равны
print(var_test_likes)

var_test_reposts = leveneTest(Reposts ~ Type, experiment_filter) # p-value = 0.9206 => дисперсии примерно равны
print(var_test_reposts)

var_test_comments = leveneTest(Comments ~ Type, experiment_filter) # p-value = 0.9192 => дисперсии примерно равны
print(var_test_comments)

# Нормальность распределения - Лайки / Репосты / Комментарии
shapiro.test(experiment_filter$Likes) # W = 0.72215, p-value < 2.2e-16 => данные не распределены по Гауссу
shapiro.test(experiment_filter$Reposts) # W = 0.60244, p-value < 2.2e-16 => данные не распределены по Гауссу
shapiro.test(experiment_filter$Comments) # W = 0.71969, p-value < 2.2e-16 => данные не распределены по Гауссу

#### Дисперсионный анализ #### 

# Параметрический тест ()
aov(Likes ~ Type, data = experiment_filter, var.equal = TRUE) %>% report() # p-value = 0.6869 => ср.значения не отличаются друг от друга
aov(Reposts ~ Type, data = experiment_filter, var.equal = TRUE) %>% report() # p-value = 0.9206 => ср.значения не отличаются друг от друга
aov(Comments ~ Type, data = experiment_filter, var.equal = TRUE) %>% report() # p-value = 0.9062 => ср.значения не отличаются друг от друга

# Непараметрический тест (Критерий Краскела-Уоллиса)
kruskal.test(Likes ~ Type, data = experiment_filter) # p-value = 0.8388 => ср.значения не отличаются друг от друга
kruskal.test(Reposts ~ Type, data = experiment_filter) # p-value = 0.7771 => ср.значения не отличаются друг от друга
kruskal.test(Comments ~ Type, data = experiment_filter) # p-value = 0.6677 => ср.значения не отличаются друг от друга

#### Хи-квадрат #### 

# Модифицируем целевые переменные
experiment_filter$likes_new <- ifelse(experiment_filter$Likes %in% c(1, 2), "Низкая вовлеченность", ifelse(experiment_filter$Likes %in% c(3, 4, 5), "Средняя вовлеченность", "Высокая вовлеченность"))
experiment_filter$reposts_new <- ifelse(experiment_filter$Reposts %in% c(1, 2), "Низкая вовлеченность", ifelse(experiment_filter$Reposts %in% c(3, 4, 5), "Средняя вовлеченность", "Высокая вовлеченность"))
experiment_filter$comments_new <- ifelse(experiment_filter$Comments %in% c(1, 2), "Низкая вовлеченность", ifelse(experiment_filter$Comments %in% c(3, 4, 5), "Средняя вовлеченность", "Высокая вовлеченность"))

# Проведем тест
chisq.test(experiment_filter$Type, experiment_filter$likes_new, correct = FALSE) # p-value = 0.8332
chisq.test(experiment_filter$Type, experiment_filter$reposts_new, correct = FALSE) # p-value = 0.3076
chisq.test(experiment_filter$Type, experiment_filter$comments_new, correct = FALSE) # p-value = 0.9502
