
library(tidyverse)
library(foreign)


tab_demos_enadid <- read.dbf("data/enadid/base_datos_enadid14_dbf/TSDem.dbf") %>% 
  as_tibble() %>% 
  mutate(EDAD = parse_number(EDAD))
tab_demos_enadid
cache("tab_demos_enadid")

tab_muj1 <- read.dbf("data/enadid/base_datos_enadid14_dbf/TMMujer1.dbf") %>% 
  as_tibble() %>% 
  mutate(EDAD_1AG = factor(EDAD_1AG, 
                           c("04","05","06","07","08","09",
                             "10","11"),
                           c("15 a 19","20 a 24","25 a 29",
                             "30 a 34","35 a 39","40 a 44",
                             "45 a 49","50 a 54")),
         edad_num = parse_number(P5_2),
         algunavez_emb = (P5_6_AG != 4), 
         P5_23 = parse_number(P5_23),
         P5_4_rec = factor(P5_4, 
                       c("00","01","02","03","04","05","06",
                         "07","08","09","10","11","12","13", "99"), 
                       c("Si asiste actualmente a la escuela",
                         "Se embarazo o tuvo un hijo",
                         "Se caso o unió",
                         "Familia o padres no la dejaron seguir estudiando",
                         "Quehaceres del hogar o cuidar algún familiar",
                         "Reprobó por bajo aprovechamiento",
                         "Problemas personales con maestros o compañeros",
                         "No había escuela, estaba lejos o no había cupo",
                         "Por falta de dinero o recursos",
                         "No quiso o no le gustó estudiar",
                         "Tenía que trabajar o entró a trabajar",
                         "Logro su meta educativa",
                         "Otro motivo",
                         "Nunca ha ido a la escuela", "No especificado") )) 
tab_muj1
cache('tab_muj1')





