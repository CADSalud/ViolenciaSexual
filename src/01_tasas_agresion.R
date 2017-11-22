

library(ProjectTemplate)
load.project()

source("munge/01_envin.R")

# TASA DE AGRESIÓN ----

# 2015 (endireh)

# tasa de agresión de 15 a 19 ----
tab_tardia <- tab_sec_x %>% 
  dplyr::select(starts_with("FAC_"), 
                P10_1_3, 
                SEXO, EDAD, edad_num) %>% 
  mutate(edad_gpo = cut(edad_num, include.lowest = T,
                        breaks = c(15, 19, 100)),
         agresion = factor(P10_1_3, 
                1:4, 
                c("muchas veces", "pocas veces", "una vez", "no ocurrió")),
         violacion = agresion != "no ocurrió")

tab_tardia %>% 
  group_by(edad_gpo, violacion) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo) %>% 
  mutate(prop = nfac/sum(nfac))
  
tab_tardia %>% 
  filter(violacion) %>% 
  group_by(edad_gpo, agresion) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo) %>% 
  mutate(prop = nfac/sum(nfac))

# infancia y adolescencia temprana menos de 15 ----
tab_temp <- tab_sec_xi %>% 
  dplyr::select(starts_with("FAC_"), 
                P11_12_5, 
                SEXO, EDAD, edad_num) %>% 
  mutate(agresion = factor(P11_12_5, 
                           1:2, 
                           c("sí", "no")) )
tab_temp %>% 
  group_by(agresion) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))
  


# 2016 (envipe) ----
# 18 años
tab_envipe <- tab_vic2 %>% 
  filter(situacion == "14") %>% 
  mutate(SEXO = factor(SEXO, 1:2, c("H", "M")), 
         edad_grupo = cut_width(edad_num, width = 5))

# tasa general
tab_envipe %>% 
  group_by(ocurrió) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE))) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))

# tasa por sexo
tab_envipe %>% 
  group_by(ocurrió, SEXO) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE))) %>% 
  group_by(ocurrió) %>% 
  mutate(prop = nfac/sum(nfac))

tab_envipe %>% 
  filter(ocurrió == 1) %>% 
  group_by(num_veces) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE)))

tab_vic2 %>% 
  mutate
  filter(situacion == "14") %>% 
  mutate()
  group_by(ocurrió) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE)))


# 2007 (envin) ----
# numero de mujeres 15 a 24 años
(tab_mjov2$FAC_PER %>% sum) # 14 millones mujeres entre 15 y 24 años

# algunas vez obligaron
tab_mjov2 %>% 
  group_by(P9_6) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER))

# varias veces
tab_mjov2 %>% 
  group_by(P9_7) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER))

# edad de primera vez obligaron
tab_mjov2 %>%
  mutate(edad_gpo = cut(parse_number(P9_9),
                        right = T,
                        breaks = c(0, 9, 14, 19, 100), 
                        include.lowest = T)) %>% 
  group_by(edad_gpo) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER))

# alguna vez embarazo
tab_mjov2 %>% 
  mutate(edad_gpo = cut(parse_number(P9_9),
                                    right = T,
                                    breaks = c(0, 9, 14, 19, 100), 
                                    include.lowest = T)) %>% 
  group_by(edad_gpo, P9_12) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) 

# embarazo sólo una persona 
tab_mjov2 %>% 
  filter(P9_12 == 1) %>% 
  data.frame()
