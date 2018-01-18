

library(tidyverse)
library(foreign)


# ENVIPE 2017
tab_demos_envipe <- read.dbf("data/envipe/bd_envipe2017_dbf/TSDem.dbf") %>% 
  as_tibble()
tab_demos_envipe %>% head


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

names(tab_vic2)
names(tab_modvic)


cache("tab_vic2")
cache("tab_modvic")
cache("tab_demos_envipe")



# 2013 a 2017

files <- list.files("data/envipe/")
files.bd <- files[str_detect(files, "^bd_envipe")]
df_vic2_envipe <- lapply(files.bd, function(file.nom){
  print(file.nom)
  
  read.dbf(paste0("data/envipe/", file.nom, "/TPer_Vic2.dbf")) %>% 
    as_tibble() %>% 
    dplyr::select(UPM, VIV_SEL, HOGAR, R_SEL,
                  FAC_ELE,
                  starts_with("AP7_")) %>%
    left_join(read.dbf(paste0("data/envipe/", file.nom, "/TSDem.dbf")) %>% 
                as_tibble() %>% 
                dplyr::select(UPM, VIV_SEL, HOGAR, R_SEL = N_REN, 
                              SEXO, EDAD), 
              by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) %>% 
    gather(preg, value, c(starts_with("AP7_3"), starts_with("AP7_4"))) %>% 
    separate(preg, c('seccion', 'preg_num', 'situacion'), sep = "_") %>% 
    mutate(preg_num = factor(preg_num, 3:4, c("ocurrió", "num_veces"))) %>% 
    spread(preg_num, value) %>% # 1 = Sí
    mutate(edad_num = parse_number(EDAD), 
           FAC_ELE = parse_number(FAC_ELE), 
           file.nom = file.nom, 
           year = parse_number(file.nom)-1)
  }) %>% 
  bind_rows()

df_vic2_envipe
df_vic2_envipe %>% summary


cache("df_vic2_envipe")
write.csv(df_vic2_envipe, "doc/df_vic2_envipe.csv", row.names = F)


df_vic2_envipe %>% 
  filter(year == 2016) %>% 
  write.csv("doc/df_vic2_envipe_2016.csv", row.names = F)

