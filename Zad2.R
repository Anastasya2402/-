# Звягина А.Д. постройте картосхему средних объемов стволов деревьев родов Липа и Каштан
# очистим полностью память
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("C:/ModInf2/zad2")

# Установим необходимые пакеты (если не установлены)
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")

library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# очистим полностью память
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("C:/ModInf2/zad2")

#считаем данные в переменные
greendb= read.csv("greendb.csv"); greendb
map=sf :: read_sf("moscow.geojson")

# график с заливкой
ggplot(map)+geom_sf(aes(fill=NAME))+theme(legend.position="none")

# переменные
spec=greendb$species_ru
genus=stringr::str_split(spec, pattern=" ",simplify=T)[,1]
data=greendb%>%mutate(Genus=genus)

# средний объем стволов
sr=data %>% group_by(adm_region,Genus)%>% 
  summarise(s_r=mean(d_trunk_m), na.rm = T)%>% 
  filter(Genus %in% c("Липа","Каштан"))
sr
sr=pivot_wider(sr,names_from = Genus, values_from = s_r)

# Объединяем данные с картой
map=map %>% mutate(adm_region=NAME)
map=left_join(map, sr, by="adm_region")

# Построение картосхемы для Липы
ggplot(map)+
  geom_sf(aes(fill=`Липа`))+theme()

# Построение картосхемы для Каштана
ggplot(map)+
  geom_sf(aes(fill=`Каштан`))+theme()
