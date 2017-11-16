
# ENVIPE 2017

library(tidyverse)
library(foreign)

tab_demos <- read.dbf("data/envipe/bd_envipe2017_dbf/TSDem.dbf") %>% 
  as_tibble()
tab_demos %>% head


tab_vic2 <- read.dbf("data/envipe/bd_envipe2017_dbf/TPer_Vic2.dbf") %>% 
  as_tibble() %>% 
  dplyr::select(ID_VIV:CVE_MUN, 
                FAC_HOG:UPM_DIS,
                starts_with("AP7_")) %>% 
  gather(preg, value, c(starts_with("AP7_3"), starts_with("AP7_4"))) %>% 
  separate(preg, c('seccion', 'preg_num', 'situacion'), sep = "_") %>% 
  mutate(preg_num = factor(preg_num, 3:4, c("ocurrió", "num_veces"))) %>% 
  spread(preg_num, value) %>% # 1 = Sí
  mutate(edad_num = parse_number(EDAD))
tab_vic2 %>% names()
tab_vic2 %>% data.frame %>% head
tab_vic2 %>% summary()


tab_modvic <- read.dbf("data/envipe/bd_envipe2017_dbf/TMod_Vic.dbf") %>% 
  as_tibble()
tab_modvic %>% names()
tab_modvic %>% data.frame %>% head

names(tab_vic1)
names(tab_vic2)
names(tab_modvic)
