

library(ProjectTemplate)
load.project()
library(bigrquery)

tab_indicadores_sgmc <- list()

load("cache/tab_sec_xii.RData")
load("cache/tab_muj1.RData")
load("cache/tab_demos_enadid.RData")
load("cache/poblacion.RData")

tab_pob <- poblacion %>% 
  filter(edad >= 10 & edad <= 19, 
         sexo == "Mujeres", 
         anio >= 2010 & anio <= 2016) %>% 
  mutate(edad_madn = cut(edad, breaks = c(10, 14, 19), 
                        labels = c("10 a 14", "15 a 19"),
                        include.lowest = T), 
         year = parse_number(anio)) %>% 
  group_by(year, edad_madn) %>% 
  summarise(pob_conapo = sum(pob)) %>% 
  ungroup()
tab_pob


# Tabla de indicadores ----

# Nacimientos ----
query_exec("SELECT * FROM [acoustic-field-186719:nacimientos.nacim2010] limit 3",
           project = "acoustic-field-186719")

tab_edad_padn <- lapply(2010:2016, function(year.nom){
  print(year.nom)
  sql <- paste0("SELECT AVG(ANO_REG), MIN(EDAD_MADN), MAX(EDAD_MADN), COUNT(filenom), ", 
                "AVG(EDAD_PADN), MIN(EDAD_PADN), MAX(EDAD_PADN) ",
                "FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] where EDAD_MADN > 14 and EDAD_MADN < 20 and EDAD_PADN < 99 ")
  tt_a <- query_exec(sql, project = "acoustic-field-186719")
  
  sql <- paste0("SELECT AVG(ANO_REG), MIN(EDAD_MADN), MAX(EDAD_MADN), COUNT(filenom), ", 
                "AVG(EDAD_PADN), MIN(EDAD_PADN), MAX(EDAD_PADN) ",
                "FROM [acoustic-field-186719:nacimientos.nacim", 
                year.nom,
                "] where EDAD_MADN > 9 and EDAD_MADN < 15 and EDAD_PADN < 99 ")
  tt_b <- query_exec(sql, project = "acoustic-field-186719")
  
  tt_a %>% bind_rows(tt_b)
  }) %>% 
  bind_rows() %>% 
  as.tibble() %>% 
  rename(year = f0_, 
         nacims = f3_, edad_padn_avg = f4_) %>% 
  unite(edad_madn, c("f1_", "f2_"), sep = " a ") %>% 
  left_join(tab_pob, 
            by = c("year", "edad_madn")) %>% 
  mutate(nacims_mil = 1000*nacims/pob_conapo)
tab_edad_padn 
  
tab_indicadores_sgmc$nacims_inds <- tab_edad_padn %>% 
  arrange(edad_madn, year)


# Edad promedio primera relacion ----

# endireh
tab_a <- tab_sec_xii %>% 
  filter(edad_num >=10 & edad_num <= 19) %>% 
  mutate(edad_primerarel = ifelse(parse_number(P12_6) %in% c(0, 999, 99, 98), 
                                  NA, 
                                  parse_number(P12_6)),
         edad_gpo = cut(edad_num, breaks = c(10, 14, 19), 
                        labels = c("10 a 14", "15 a 19"),
                        include.lowest = T)) %>% 
  mutate(prop = sum(FAC_MUJ*as.numeric(!is.na(edad_primerarel))) /sum(FAC_MUJ))  %>%
  filter(!is.na(edad_primerarel)) %>% 
  group_by(edad_gpo) %>% 
  summarise(encuesta = "endireh 2016", 
            prom = sum(FAC_MUJ*edad_primerarel)/sum(FAC_MUJ), 
            prop = unique(prop))

tab_a

tab_b <- tab_muj1 %>% 
  filter(edad_num >=10 & edad_num <= 19) %>% 
  mutate(edad_primerarel = ifelse(parse_number(P8_36) >= 60, 
                                  NA, 
                                  parse_number(P8_36)),
         edad_gpo = cut(edad_num, breaks = c(10, 14, 19), 
                        labels = c("10 a 14", "15 a 19"),
                        include.lowest = T)) %>% 
  mutate(prop = sum(FAC_PER*as.numeric(!is.na(edad_primerarel))) /sum(FAC_PER))  %>%
  filter(!is.na(edad_primerarel)) %>% 
  group_by(edad_gpo) %>% 
  summarise(encuesta = "enadid 2014",
            prom = sum(FAC_PER*edad_primerarel)/sum(FAC_PER), 
            prop = unique(prop))
tab_b
  
tab <- tab_a %>% 
  bind_rows(tab_b)
tab_indicadores_sgmc$edad_primerarelsex <- tab


# embarazadas ----

# endadid 2014
# P5_6 # embarazada alguna vez
# P7_1 # embarazada actualmente
# P5_6_AG # 2, 3: actualmemte, 4: nunca emb
# P5_22 # alguna perdida o aborto
# P5_18 # mortinatos

tab <- tab_muj1 %>% 
  filter(edad_num >=10 & edad_num <= 19) %>% 
  mutate(edad_gpo = cut(edad_num, breaks = c(10, 14, 19), 
                        labels = c("10 a 14", "15 a 19"),
                        include.lowest = T)) %>% 
  dplyr::select(edad_gpo, edad_num, FAC_PER,
                P5_6, P7_1, P5_6_AG, P5_22, P5_18) %>% 
  mutate(emb_algvez = as.numeric(P5_6 == 1),
         emb_actual = as.numeric(P7_1 == 1), 
         aborto = as.numeric(P5_22 == 1), 
         mortinato = as.numeric(P5_18 == 1), 
         perdida = as.numeric((aborto + mortinato)>0)) %>% 
  group_by(edad_gpo) %>% 
  summarise(encuesta = "enadid 2014",
            embalgv = sum(emb_algvez*FAC_PER), 
            embalgvp = embalgv/sum(FAC_PER), 
            embact = sum(emb_actual*FAC_PER),
            embactp = embact/sum(FAC_PER),
            abortoalgv = sum(perdida*FAC_PER, na.rm = T),
            abortopemb = abortoalgv/embalgv) 
tab

tab_indicadores_sgmc$embarazo_aborto_inds <- tab


# estado civil ----

# enadid
tab <- tab_muj1 %>% 
  filter(edad_num >=10 & edad_num <= 19) %>% 
  mutate(edad_gpo = cut(edad_num, breaks = c(10, 14, 19), 
                        labels = c("10 a 14", "15 a 19"),
                        include.lowest = T)) %>%
  left_join(tab_demos_enadid) %>% 
  dplyr::select(FAC_PER, edad_num, edad_gpo, 
                P10_1_AG, P3_20_AG) %>% 
  mutate(unida = P10_1_AG != 3, 
         casalgvz = P3_20_AG %in% c(2, 3, 4), 
         nosoltera = as.numeric((unida + casalgvz) >0)) %>% 
  group_by(edad_gpo) %>% 
  summarise(encuesta = 'endadid 2014',
            nosolt = sum(FAC_PER*nosoltera),
            nosoltp = nosolt/sum(FAC_PER))
tab

# 2	Casada(o)
# 3	En unión libre
# 4	Separada(o), divorciada(o) o viuda(o)



tab_indicadores_sgmc$matrimonio_precoz <- tab

# saving ----
tab_indicadores_sgmc
cache("tab_indicadores_sgmc")

