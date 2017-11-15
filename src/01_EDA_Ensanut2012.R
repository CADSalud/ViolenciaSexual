library(ProjectTemplate)
load.project()
library(tidyverse)
library(Hmisc)
library(janitor)

#Base de datos ENSANUT 2012 - Integrantes del hogar
#Carga de base de datos

#CUESTIONARIO HOGAR
#12 a 19 años de edad

# save SPSS dataset in trasport format
hogar <- spss.get("data/ENSANUT2012/integrantes.sav", 
                  use.value.labels=TRUE,reencode="LATIN1")
head(hogar)

summary(hogar)

#II. Características demográficas.
# using subset function 
#hogar_adol <- subset(hogar, edad <= 19 & edad >=10, 
                  #select=c(folio,intp, entidad, munici, edad, sexo, h219))
#summary(hogar_adol)

# using pipes
hogar_adol <- hogar %>% 
  as_tibble() %>% 
  filter(edad <= 19 & edad >=12 & !is.na(h219)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, h219,pondei) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         sol219 = ifelse(h219=="está soltero(a)?",1,0),
         scony=ifelse(sol219==1,"solt", "nsolt"))

table(hogar_adol$h219,hogar_adol$edad)

hogar_adol %>% 
  group_by(redad,scony) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

hogar_adol %>% 
  group_by(scony) %>% 
  summarise(casos = round(sum(pondei),0)) 
  

hogar_adol %>% 
  group_by(redad,scony,sexo) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

hogar_adol %>% 
  group_by(redad,scony,entidad) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad,entidad) %>% 
  mutate(total_edad = round(sum(casos),0),
         total_ent = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

# PEGAR BASES DE DATOS DE DIEGO VALLE PARA CONOCER TASA 
# POR CADA MIL HABITANTES DEL RANGO DE EDAD SELECCIONADO Y NOMBRES DE ENTIDADES
# VERIFICAR EL TEMA DE REPRESENTATIVIDAD
# COMPARAR LOS TOTALES ESTIMADOS POR GRUPO ETARIO Y ENTIDAD PARA VER DIFERENCIAS

#III. Situación de salud y utilización de servicios de salud.
# using pipes
#Enfermedades de transmisión sexual
salud_sex <- hogar %>% 
  filter(edad <= 19 & edad >=12 & !is.na(h311)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, h301,h302,h311,h312,pondei)
summary(salud_sex)
#0 casos
#Los adolescentes no reportan tener enfermedades de transmisión sexual en las últimas dos semanas

#Fueron al hospital por partos o problemas por embarazo en el último año
hogar_hosp <- hogar %>% 
  filter(edad <= 19 & edad >=12) %>% 
  select(folio,intp, entidad, munici, edad, sexo, h301,h302,h311,h312,pondei) %>%
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         hosp_emb=ifelse(h311=="Sí" & (h312=="Parto"| h312=="Cesárea"),"si", "no"))
summary(hogar_hosp)
table(hogar_hosp$hosp_emb)

hogar_hosp %>% 
  group_by(redad,hosp_emb) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

hogar_g<-hogar_adol %>% 
  group_by(redad,scony,entidad) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad,entidad) %>% 
  mutate(total_edad = round(sum(casos),0),
         total_ent = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))%>%
  filter(scony=="nsolt")

ggplot(subset(hogar_g,redad %in% c("adol_tard" , "adol_temp"))) + 
  geom_col(aes(hogar_g$entidad, hogar_g$porc_edad, group=redad, fill=redad)) +
  theme_bw()

# Definición de hacinamiento más común, + 2.5 personas en un cuarto.
# No encontré la pregunta 5.05 y 5.06 relacionada con hacinamiento
# La marginación sí viene

#CUESTIONARIO ADOLESCENTES
#10 a 19 años de edad

adol <- spss.get("data/ENSANUT2012/Adolescentes.sav", 
                  use.value.labels=TRUE,reencode="LATIN1")
head(adol)

#2.06 ¿A qué edad tuviste tu primera relación sexual?
#00 no ha tenido
#88 no recuerda
#99 ns/nr

table(adol$d206)

adol_sex <- adol %>% 
  as_tibble() %>% 
  filter(edad <= 19 & edad >=10 & !is.na(d206)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, d206, d207,d212, d212a, d213, pondef) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         sinexp = ifelse(d206!=0,1,0),
         edadp = ifelse(d207<15,0,ifelse(d207>14 & d207<20,1,2)))

total<-adol_sex %>% 
  group_by(redad,sexo) %>% 
  summarise(total = sum(pondef))

sum(total$total)

table(adol_sex$sinexp)
table(adol_sex$edadp)

adol_sex %>% 
  group_by(redad,sinexp) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

adol_sex %>% 
  filter(!is.na(edadp)) %>%
  group_by(redad,edadp) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))


table(adol$d207)



