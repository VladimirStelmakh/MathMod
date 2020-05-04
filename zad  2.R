#Стельмах Владимир ПАЭ 121
#создайте модель множественной линейной регрессии ночных потоков паров воды 
#за период 2013 года по данным измерений методом турбулентной пульсации
rm(list=ls())
library("tidyverse")
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble")
library("readr") 
library(rnoaa)
library(lubridate)

#Загрузка и редакция исходных данных
data = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("[")) 
data = data[-1,]
# отбираем данные за 2013 год
data = data[year(data$date) == 2013, c(1:ncol(data))]
# отбираем данные ночных потоков
data=data[data$daytime == FALSE,] 
glimpse(data)
# удаляем пустой и ненужный столбец
data = select(data, -(roll)) 
#делаем преобразование строковых значений в факторные
data = data %>% mutate_if(is.character, factor) 
#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(data) = names(data) %>% str_replace_all("[!]","_emph_") %>% 
  #Переименуем столбцы для корректных названий для переменных
  str_replace_all("[?]","_quest_") %>%  
  str_replace_all("[*]","_star_") %>%  
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(data)
#выберем все переменные типа numeric
data_numeric = data[,sapply(data,is.numeric) ] 
data_non_numeric = data[,!sapply(data,is.numeric) ]
cor_td = cor(drop_na(data_numeric)) %>% as.data.frame %>% select(h2o_flux) 
#выберем имена переменных (строк) коэффициентом детерминации больше 0.1
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
#собераем переменные из вектора в формулу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""));formula 
#Создадим  выборки
row_numbers = 1:length(data_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(data_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = data_numeric[teach,]
#Тестирующая выборка
testing_tbl = data_numeric[test,]

#Модель 1
#создаем модель линейной регрессии
model = lm(formula, data = data);model 
formula = h2o_flux ~ (rand_err_Tau + LE + qc_LE + rand_err_LE + h2o_flux + 
                        qc_h2o_flux + rand_err_h2o_flux + h2o_time_lag + sonic_temperature + 
                        air_temperature + air_density + air_molar_volume + es + RH + 
                        VPD + u_star_ + TKE + un_LE + un_h2o_flux + u_var + v_var + 
                        w_var + w_div_h2o_cov + flowrate)
coef(model)
resid(model)
confint(model)
summary(model)
anova(model)
plot(model)

#Модель 2
formula2 = h2o_flux ~ (rand_err_Tau + LE + qc_LE + rand_err_LE +  
                         rand_err_h2o_flux + h2o_time_lag + sonic_temperature + 
                         air_temperature + air_density + air_molar_volume + es + RH + 
                         VPD + u_star_ + TKE + un_LE + un_h2o_flux +w_div_h2o_cov)
model2 = lm(formula2, data = data);model2 #
anova(model2)
summary(model2)
anova(model2)
plot(model2)
#Модель 3
formula3 = h2o_flux ~ (LE + air_density + air_molar_volume + es + RH + 
                         VPD + un_LE + un_h2o_flux +w_div_h2o_cov)
model3 = lm(formula3, data = data);model3 
anova(model3)#
summary(model3)
anova(model3)
summary(model3)
plot(model3)

#Модель4
formula4 = h2o_flux ~ (LE + RH + VPD + un_LE + un_h2o_flux +w_div_h2o_cov)
model4 = lm(formula4, data = data);model4 
model4 = lm(formula4, data = data)
anova(model4)
summary(model4)
plot(model4)