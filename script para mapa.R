#1. Carga de paquetes y WD ----
rm(list = ls())
library(tidyverse)
library(readxl)
library(sf)
library(magrittr)
library(mapview)
library(printr)
#install.packages("printr")
#install.packages("googledrive")
setwd("C:/Users/Daniel/Desktop/CAMP_SOLID/4 junio")
getwd() #espacio de trabajo actualizado
list.files()
#https://docs.google.com/spreadsheets/d/e/2PACX-1vQawLKRruyLNHg1LSMjfxt0siwuj9ng_wxQlDM-a3aWUD1Z4LiVIP9E8lfrvAywQQUaluxVGFMlZhx5/pub?gid=1371277324&single=true&output=csv
myurl <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQawLKRruyLNHg1LSMjfxt0siwuj9ng_wxQlDM-a3aWUD1Z4LiVIP9E8lfrvAywQQUaluxVGFMlZhx5/pub?gid=1371277324&single=true&output=csv"
wti <- read_csv(url(myurl))
head(wti)
tail(wti)

data_1 <- wti
#2. Lectura e importacion de datos ----

#sheets <- readxl::excel_sheets("CATASTRO INTERACTIVO.xlsx")
#data_1 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[1])
#data_2 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[2])
#data_3 <- readxl::read_excel("CATASTRO INTERACTIVO.xlsx", sheets[3])

#3. Chequeo de datos ----
summary(data_1)
glimpse(data_1)

#4. coercion de coordenadas LONGITUD y LATITUD y transformaci?n a sf

# las coordenadas geograficas vienen con errores de formato
# para arreglarlas, extraje los primeros 3 valores,
# y lo pegue (o concatene) con los valores de la 4ta posici?n al ultimo, con una coma al medio
# esto para colocar el punto decimal donde corresponde.

#las coordenadas buenas est?n en las columnas lon_x y lat_y.
data_1 %<>%
  mutate(lon_x = as.double(paste0(str_sub(LONGITUD, start = 1, end = 3), ".",
                                  str_sub(LONGITUD, start = 4))),
         lat_y = as.double(paste0(str_sub(LATITUD, start = 1, end = 3), ".",
                                  str_sub(LATITUD, start = 4))))
data_1 <- data_1 %>% 
  rename(ID_FAMILIA = ID,
         AYUDA_RECIBIDA = AYUDA_ENTREGADA)
colnames(data_1)
##3.1. Seleccionar "FAMILIA AYUDADA" Y "FAMILIA POR AYUDAR"----
familias_apoyadas <- data_1 %>% 
  select(DESCRIPCION, ID_FAMILIA, COMUNA, INTEGRANTES_GRUPO_FAMILIAR, 
         NACIONALIDAD, AYUDA_RECIBIDA, FECHA_AYUDA, lon_x, lat_y) %>%
  view()
count(familias_apoyadas) #82

colnames(familias_apoyadas)
sum(is.na(familias_apoyadas$INTEGRANTES_GRUPO_FAMILIAR)) #15 familias que no se conoce el n°integrantes

#Exportar base de datos data_1
write.csv(familias_apoyadas, file = "FAMILIAS_APOYADAS.csv", row.names = F)
#4. Estadisticas#####----
#Promedio de integrantes por familia (3,069), Min: 1 Max:9
select_not_NA <- data_1 %>% 
  filter(!is.na(INTEGRANTES_GRUPO_FAMILIAR)) 
summary(select_not_NA$INTEGRANTES_GRUPO_FAMILIAR)
#Familias ayudadas por comuna
datos_por_comuna <- data_1 %>% 
  count(COMUNA) %>% rename(count = n) %>% 
  view()
datos_por_comuna2 <- data_1 %>% group_by(COMUNA, NACIONALIDAD) %>% summarise(familias_beneficiadas = n()) %>% View()
unique(data_1$COMUNA)
#Familias ayudadas por COMUNA y NACIONALIDAD
datos_nacionalidad <- data_1 %>% 
  count(COMUNA, NACIONALIDAD) %>% 
  view()

#ESTADISTICAS DE LAS PERSONAS BENEFICIADAS EN LO PRADO SEGUN NACIONALIDAD
familias_apoyadas_LP2 <- familias_apoyadas %>%
  group_by(COMUNA = "Lo Prado", NACIONALIDAD) %>% 
  summarise(Personas_beneficiadas = sum(INTEGRANTES_GRUPO_FAMILIAR, na.rm= T)) %>%
  #summarise(n_casos = n()) %>% 
  ungroup() %>% 
  view()
#FAMILIAS APOYADAS POR COMUNA Y FECHA
familias_apoyadas <- familias_apoyadas %>% mutate(DELIVER_DATE = dmy(FECHA_AYUDA))
names(familias_apoyadas)
datos_nacionalidad <- familias_apoyadas %>% 
  count(COMUNA, FECHA_AYUDA) %>% arrange(desc(FECHA_AYUDA)) %>% 
  view()
#SACAR ESTADISTICAS DE LAS FAMILIAS AYUDADAS POR COMUNA
familias_apoyadas_comuna <- familias_apoyadas %>%
  group_by(COMUNA) %>% 
  #group_by(COMUNA, NACIONALIDAD, AYUDA_RECIBIDA, INTEGRANTES_GRUPO_FAMILIAR) %>% 
  #summarise(familias_ayudadas = sum(COMUNA), na.rm=T) %>%
  summarise(familias_ayudadas = n()) %>% 
  ungroup() %>% 
  view()

#funci?n para sacar sumario en word
LP2 <- summary(familias_apoyadas_LP2)
capture.output(LP2, file = "Sumario_LP2.doc")

#5. Transformación a shp----
#Aqui transformo el objeto data_1 (data.frame) a un ojeto sf, que se utiliza para guardar informacion geoespacial
# es como un shape pero en R
data_1_sf <- st_as_sf(data_1,
                      coords = c("lon_x", "lat_y"),
                      crs = 4326)

# Con esto exportas el objeto sf a un shapefile externo
st_write(obj = data_1_sf, dsn = "data_1_sf.shp", driver = "ESRI Shapefile", delete_dsn = T)



#5. Visualizacion
ggplot() +
  geom_sf(data = data_1_sf)

mapview(data_1_sf)
