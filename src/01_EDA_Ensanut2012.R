library(ProjectTemplate)
library(tidyverse)
library(Hmisc)
library(janitor)
load.project()

#Base de datos ENSANUT 2012 - Integrantes del hogar
#Carga de base de datos

#CUESTIONARIO HOGAR
#12 a 19 años de edad

# save SPSS dataset in trasport format
hogar <- spss.get("data/ENSANUT2012/integrantes.sav", 
                  use.value.labels=TRUE,reencode="LATIN1")
head(hogar)
summary(hogar)

load("cache/poblacion.RData")

pob2012 <- poblacion %>% 
  filter(edad <= 19 & edad >=12 & anio = "2012") %>% 
  select(edad,pob) 


#II. Características demográficas.
# using subset function 
#hogar_adol <- subset(hogar, edad <= 19 & edad >=10, 
                  #select=c(folio,intp, entidad, munici, edad, sexo, h219))
#summary(hogar_adol)

# Situación conyugal
hogar_adol <- hogar %>% 
  filter(edad <= 19 & edad >=12 & !is.na(h219)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, h219,pondei) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         sol219 = ifelse(h219=="está soltero(a)?",1,0),
         scony=ifelse(sol219==1,"solt", "nsolt"))

total_muestra<-hogar_adol %>% 
  group_by(redad,sexo) 
  
table(total_muestra$redad,total_muestra$sexo)

total_hogar<-hogar_adol %>% 
  group_by(redad,sexo) %>% 
  summarise(total = sum(pondei))

table(hogar_adol$h219,hogar_adol$edad)

#Genera tabla de situación conyugal filtrando variables que no usamos
hogar_adol %>% 
  group_by(redad,scony) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

#Total adolescentes entre 10 y 19 años solteros y no solteros
hogar_adol %>% 
  group_by(scony) %>% 
  summarise(casos = round(sum(pondei),0)) 
  
#Adolescentes por rango de edad y sexo, solteros y no solteros
hogar_adol %>% 
  group_by(redad,scony,sexo) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

#Adolescentes por rango de edad y sexo, solteros y no solteros, por entidad
no_solter_entidad <- hogar_adol %>% 
  group_by(redad,scony,entidad) %>% 
  summarise(casos = round(sum(pondei),0)) %>% 
  ungroup() %>% 
  group_by(redad,entidad) %>% 
  mutate(total_edad = round(sum(casos),0),
         total_ent = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

write.csv(no_solter_entidad, "data/ENSANUT2012/no_solter_entidad.csv")


#III. Situación de salud y utilización de servicios de salud.
# using pipes
#Enfermedades de transmisión sexual

salud_sex <- hogar %>% 
  filter(edad <= 19 & edad >=12 & !is.na(h311)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, h301,h302,h311,h312,pondei)
summary(salud_sex)
#0 casos
#Los adolescentes no reportan haber ido al hospital por enfermedades de 
#transmisión sexual en las últimas dos semanas

#Fueron al hospital por partos o problemas por embarazo en el último año
hogar_hosp <- hogar %>% 
  filter(edad <= 19 & edad >=12 & sexo=="Mujer") %>% 
  select(folio,intp, entidad, munici, edad,h301,h302,h311,h312,pondei) %>%
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



# Definición de hacinamiento más común, + 2.5 personas en un cuarto.
# No encontré la pregunta 5.05 y 5.06 relacionada con hacinamiento
# La marginación sí viene

################CUESTIONARIO ADOLESCENTES
#10 a 19 años de edad

adol <- spss.get("data/ENSANUT2012/Adolescentes.sav", 
                  use.value.labels=TRUE,reencode="LATIN1")
head(adol)


#d206
#2.06 ¿A qué edad tuviste tu primera relación sexual?
#00 no ha tenido
#88 no recuerda
#99 ns/nr

table(adol$d206)

#Genera tabla filtrando variables de interés
adol_sex <- adol %>% 
  as_tibble() %>% 
  filter(edad <= 19 & edad >=10 & !is.na(d206)) %>% 
  select(folio,intp, entidad, munici, edad, sexo, d206, d207,d212, d212a, d213, pondef) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         sinexp = ifelse(d206!=0,1,0),
         edada  = ifelse(d206<15,0,1),
         edadp = ifelse(d207<15,0,ifelse(d207>14 & d207<20,1,2)))


#Nota: Filtrando NS/NC del d206 cambian los resultados!!

#Por rangos de edad, tabla edad adolescente vs edad pareja
adol_sex %>% 
  filter(d206>0 & d206<88 & d207<88) %>%
  group_by(edada,edadp) %>% 
  summarise(casos_sex_pe = round(sum(pondef),0)) %>% 
  mutate(total_cruce = round(sum(casos_sex_pe),0),
         porc_pe = round((casos_sex_pe/total_cruce)*100,2))

#tabla_edad_10a14 <- tabla_edadp[ which(tabla_edadp$edada==0),]
#tabla_edad_15a19 <- tabla_edadp[ which(tabla_edadp$edada==1),]


#Filtro de los que han tenido relaciones sexuales y los nr/ns (88,99)
prs_edad <- adol_sex %>% 
  filter(d206>0 & d206<88 & d207<88) %>% 
  select(d206,d207,sexo,pondef)  %>% 
  group_by(d206,d207,sexo) %>% 
  mutate(dif=ifelse((d207-d206)>4,1,0)) %>% 
  summarise(total = sum(pondef),d=min(dif)) 

#  group_by(d) %>%
#  summarise(otra=sum(total))

#Scatterplot entre edad adolescente y edad pareja
ggplot(prs_edad, aes(x=d206, y=d207,color=sexo)) + 
  geom_point( alpha = 0.8)+geom_abline() +geom_jitter()

prs_edad$catd206<-factor(prs_edad$d206)

ggplot(data=prs_edad, aes(x=d207)) + geom_histogram()

png(filename="graphs/prs_edadpar.png")

ggplot(aes(y = d207, x = catd206, fill = sexo), data = prs_edad) + 
  geom_boxplot() + geom_abline(intercept = 5.5) +
  labs(x = "Edad adolescente", y = "Edad pareja", title = "ENSANUT 2012")

dev.off()

summary(prs_edad$d207)
        
#Filtro de los que han tenido relaciones sexuales 
graf_sex <- adol_sex %>% 
  filter(d206>0 & d206<88) %>% 
  group_by(d206)

#summarise(casos = round(sum(pondef),0)) 

#Agrupado por edad
ggplot(data=graf_sex, aes(d206)) + 
  geom_histogram()

total<-adol_sex %>% 
  group_by(redad,sexo) %>% 
  summarise(total = sum(pondef))

sum(total$total)

table(adol_sex$sinexp)
table(adol_sex$edadp)

#Porcentaje de adolescentes que han tenido relaciones sexuales
adol_sex %>% 
  group_by(redad,sinexp) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))



table(adol$d207)

#ADOL
#d212 ¿Alguna vez has estado embarazada?
#d213 ¿Cuántos embarazos en total has tenido?
#!is.na(h219)

head(adol)

summary(adol$d212)
summary(adol$d213)



#Solo mujeres que hayan tenido relaciones sexuales (que sean diferentes de NA o NS/NR para 
#pregunta sobre embarazo)
adol_embarazo <- adol %>% 
  filter(edad <= 19 & edad >=10 & d212!="NS/NR" & sexo == "Mujer" & !is.na(d212)) %>% 
  select(folio,intp, entidad, munici, edad, d212, d213,pondef) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"),
         algemb = ifelse(d212=="Sí",1,0),
         numemb = ifelse((d213>1 & d213<88 & algemb==1),1,0))

table(adol_embarazo$d212,adol_embarazo$edad)

table(adol_embarazo$d213,adol_embarazo$edad)

adol_embarazo %>% 
  group_by(redad,algemb) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))

adol_embarazo %>% 
  group_by(redad,numemb) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total_edad = round(sum(casos),0),
         porc_edad = round((casos/total_edad)*100,2))


#Unión de base de datos situación conyugal con embarazos
hasc <- hogar_adol %>% 
  filter(sexo == "Mujer") %>% 
  select(folio,intp,scony)

hasc$folio <- as.numeric(hasc$folio)
adol_embarazo$folio <- as.numeric(adol_embarazo$folio)

vinc_sitcony_emb <- hasc %>% 
  right_join(adol_embarazo,
             by = c('folio','intp'))

write.csv(vinc_sitcony_emb, "data/ENSANUT2012/vinc_sitcony_emb.csv")

head(vinc_sitcony_emb)  

dim(vinc_sitcony_emb) 
#1825 x 12
sum(vinc_sitcony_emb$algemb)
#1030
#1030/1825 = 0.56

#Cálculo de casos para dimensionar base de datos que incluye sit conyugal y embarazos
#Resultado
#       0  1
#nsolt  2 16
#solt  62 48
table(vinc_sitcony_emb$scony,vinc_sitcony_emb$algemb) 
 
#Calcula porcentaje de embarazadas con situación conyugal
#CUIDADO, LOS RESULTADOS NO SIRVEN PORQUE EL NÚMERO ES MUY LIMITADO
vinc_sitcony_emb %>% 
  group_by(scony,algemb) %>% 
  summarise(casos=round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(scony) %>% 
  mutate(total_scony = round(sum(casos),0),
         porc_scony = round((casos/total_scony)*100,2))

###################################
###################################VIOLENCIA SEXUAL

head(adol)
#Genera tabla filtrando variables de interés
viol_sex <- adol %>% 
  as_tibble() %>% 
  filter(edad <= 19 & edad >=10 & d601!="NS/NR") %>% 
  select(folio,intp, entidad, munici, edad, sexo, d601,d602g,d602esp,d603,d603esp,d604,d604esp,pondef) %>% 
  mutate(redad=ifelse(edad<=14,"adol_temp","adol_tard"))
         
        
table(viol_sex$d601)

#Agresión sexual
viol_sex %>% 
  group_by(d601,d602g) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  mutate(total = round(sum(casos),0),
         porc = round((casos/total)*100,2))

#Motivo
viol_sex %>% 
  group_by(d601,d603) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  mutate(total = round(sum(casos),0),
         porc = round((casos/total)*100,2))

#Motivo
violacion<-  viol_sex %>% 
  group_by(d602g,d603) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  mutate(total = round(sum(casos),0),
         porc = round((casos/total)*100,2))

#Lugar de la agresión sexual
viol_sex %>% 
  group_by(d602g,d604) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  mutate(total = round(sum(casos),0),
         porc = round((casos/total)*100,2))


################CUESTIONARIO ADULTOS


adul <- spss.get("data/ENSANUT2012/Adultos.sav", 
                 use.value.labels=TRUE,reencode="LATIN1")
head(adul)

#Genera tabla filtrando variables de interés
viol_sex_adul <- adul %>% 
  as_tibble() %>% 
  filter(edad > 18 & a1201!="NS/NR") %>% 
  select(folio,intp, entidad, munici, edad, sexo, a1201, a1202g, a1203,pondef) %>% 
  mutate(redad = cut(edad, breaks = c(seq(19,65, by = 5),114), 
                        include.lowest = F))
           
          
table(viol_sex_adul$redad)

#Agresión sexual
viol_sex_adul %>% 
  group_by(redad,a1201,a1202g) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(total = round(sum(casos),0),
         porc = round((casos/total)*100))

#Motivo
tab_viol_sex_adul <- viol_sex_adul %>% 
  group_by(redad,a1201,a1203) %>% 
  summarise(casos = round(sum(pondef),0)) %>% 
  ungroup() %>% 
  group_by(redad) %>% 
  mutate(viol  = ifelse(a1203=="Violación por novio / pareja / esposo",1,
                        ifelse(a1203=="Violación por algún familiar",1,
                        ifelse(a1203=="Violación por desconocido",1,0))),
         total = round(sum(casos),0),
         porc = round((casos/total)*100)) 


# BROOM ----

porc_fun <- function(sub){
  sub  %>% 
    group_by(viol,redad) %>% 
    summarise(npond = sum(casos)) %>% 
    ungroup() %>% 
    mutate(porc = npond/sum(npond))
}

boot.porc <- tab_viol_sex_adul %>% 
  bootstrap(m = 1000) %>% 
  do( porc_fun(sub = .) )
boot.porc

porc.broom <- boot.porc %>% 
  ungroup %>% 
  filter(viol == 1) 

porc.broom %>% 
  ggplot(aes(x = porc)) + 
  geom_histogram(bins = 10) +
  facet_wrap(~redad)

  
  facet_wrap(~redad,scales = "free_x")

tab.res <- tibble( tipo = "broom", 
                   prom = mean(porc.broom$porc),
                   median = median(porc.broom$porc),
                   q975 = quantile(porc.broom$porc, .975),
                   q025 = quantile(porc.broom$porc, .025))
tab.res



  