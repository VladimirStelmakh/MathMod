#Владимир Стельмах ПАЭ 121
#Регион 8
#Для региона 8 рассчитайте урожайность пшеницы в 2000 году, взяв для рассчета средние суммы активных температур за предыдущие 7 лет, с 10 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 27 градусов
library(tidyverse)
library(rnoaa)
library(lubridate)
###Создадим векторы с данными для расчета: Р°:
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00) #константа
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00) #константа
dfi = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00) #отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
Kf = 300 # Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # сумма частей основной и побочной продукции
Ej = 25 # стандартная влажность культуры 
#скачиваем станции 
#ghcnd_stations()
setwd("/Users/vladimirstelmakh/Desktop/R2/r")
getwd()
#station_data = ghcnd_stations(station_data)
#write.csv(station_data,"station_data.csv")

station_data = read.csv("station_data.csv")
#Получим список метеостанций
ELISTA = data.frame(id = "ELISTA", latitude = 46.307743,  longitude = 44.269759)
#Найдем станции, соответствующие критериям
elista_around = meteo_nearby_stations(lat_lon_df = ELISTA, station_data = station_data,
                                    limit = 10, var = c("PRCP", "TAVG"),
                                    year_min = 1993, year_max = 2000)
# В конечном счете мы получили таблицу с идентификаторами метиостанций по удаленности от Элисты
elista_around
# Далее объединяем наши данные.Мы создаем цикл для всех метеостанций, выбрав нужное время
all_elista_data = tibble()
for (v in 1:10) 
{ 
elista_id = elista_around[["ELISTA"]][["id"]][v] 
data = meteo_tidy_ghcnd(stationid = elista_id, 
                          var="TAVG", 
                          date_min="1993-01-01", 
                          date_max="2000-12-31") 
all_elista_data = bind_rows(all_elista_data, data) 
}
### Произведем обработку полученных данных 
#Добавим для наших данных колонки year, month,для группировки
clean_data = all_elista_data %>% 
  mutate(year = year(date), month = month(date)) %>% 
  #ДАлее сгрупируем с учетом id наших метеостанций
  group_by(year, month, id) %>% 
  # суммирования с учетом id метеостанций
  summarize(tavg = sum(tavg[tavg>5], na.rm=TRUE)/10) %>% 
  # нахождения средних месячных активных температур, сгрупировав данные: 
  group_by(month) %>% 
  summarize(s = mean(tavg, na.rm = TRUE)) %>% 
  # создадим колонки для расчета: 
  mutate (a = afi, b = bfi, d = dfi) %>% 
  # и рассчитаем урожайность для каждого месяца: 
  mutate (fert = ((a + b * s) * d * Kf) / (Qj * Lj * (100-Ej)) ) 
# сумма урожайностей равна: 
Yield = sum(clean_data$fert); Yield
#Для 8 региона (Элиста) урожайность пшеницы в 2012 году составила - 21,0694 ц/га
