library(ProjectTemplate)
load.project()

### 1. Carga y totales de tablas SAEH ----
load('cache/catcie10.RData')
load('cache/egreso_adoles.RData')
load('cache/sectorial_adoles.RData')
load('cache/egreso.RData')
load('cache/sectorial.RData')

### 2. Totales de egreso y sectorial ----
nrow(egreso)    # 2,959,197
nrow(sectorial) # 6,368,082

### 3. Totales de egreso y sectorial para adolescentes ----
nrow(egreso_adoles)    # 521,649
nrow(sectorial_adoles) # 743,163

### 4. Validación sobre sectorial contiene a egreso ----
sectorial_ss <- sectorial %>% 
  filter(DH == 1)
nrow(sectorial_ss)    # 2,959,197

### 5. Porcentaje de egresos hospitalarios para adolescentes ----
nrow(sectorial_adoles)/nrow(sectorial)   # 0.1167012

### 6. Distribución de edad y sexo en egresos hospitalarios de adolescentes----
sectorial_adoles %>% 
  group_by(redad, genero) %>% 
  summarise(n=n()) %>% 
  mutate(porc = 100*n/sum(n)) %>% 
  ggplot(aes(x = fct_relevel(redad, c('adoles_temp','adoles_tard')), 
             y = porc, fill = genero, label = n)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  geom_text(color = 'black',size = 2) + 
  theme_bw() + 
  xlab('Rango de edad') + 
  ylab('Porcentaje de casos')

### 7. Ranking de padecimientos----
sectorial_adoles %>% 
  group_by(cve_afec_prin) %>% 
  summarise(n=n()) %>% 
  left_join(catcie10 %>% select(cve = CAUSA, diagnostico),
            by = c("cve_afec_prin" = "cve")) %>% 
  arrange(desc(n))

### 8. Embarazo, parto y puerperio (O00-O99) ----
adolescentes <- sectorial_adoles %>% 
  mutate(parto = ifelse(!is.na(str_extract(cve_afec_prin,pattern = 'O')),1,0))

mean(adolescentes$parto) # 0.5771977

### 9. Distribución de partos ----
adolescentes %>% 
  filter(parto == 1) %>% 
  group_by(redad) %>% 
  summarise(n=n())

sectorial_adoles %>% 
  mutate(parto = ifelse(!is.na(str_extract(cve_afec_prin,pattern = 'O')),1,0)) %>% 
  group_by(redad, parto) %>% 
  summarise(n=n()) %>% 
  mutate(porc = 100*n/sum(n)) %>% 
  ggplot(aes(x = fct_relevel(redad, c('adoles_temp','adoles_tard')), 
             y = porc, fill = as.factor(parto), label = n)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  geom_text(color = 'gray',size = 3, position = 'stack') + 
  theme_bw() + 
  xlab('Rango de edad') + 
  ylab('Porcentaje de casos')

### 10. Agresión sexual ----
agr_sex <- sectorial_adoles %>% 
  mutate(agresion = ifelse(!is.na(str_extract(cve_afec_prin,pattern = 'Y')),1,
                           ifelse(cve_afec_prin %in% c('Z044','T742'),1,0))) %>% 
  mutate(pad_agr = toupper(substr(cve_afec_prin,start = 1,stop = 1)))



agr_sex %>% 
  filter(agresion == 1) %>% 
  group_by(redad, genero) %>%
  summarise(n=n()) %>%
  mutate(porc = 100*n/sum(n)) %>%
  ggplot(aes(x = fct_relevel(redad, c('adoles_temp','adoles_tard')),
             y = porc, fill = genero, label = n)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(color = 'gray',size = 3, position = 'stack') +
  theme_bw() +
  xlab('Rango de edad') +
  ylab('Porcentaje de casos')


causas_agregadas <- data.frame(clave = LETTERS, 
                               descripcion = c("Ciertas enfermedades infecciosas y parasitarias",
                                               "Ciertas enfermedades infecciosas y parasitarias",
                                               "Neoplasias y Enfermedades de la sangre, hematopoyéticos y mecanismo de la inmunidad",
                                               "Neoplasias y Enfermedades de la sangre, hematopoyéticos y mecanismo de la inmunidad",
                                               "Enfermedades endocrinas, nutricionales y metabólicas",
                                               "Trastornos mentales y del comportamiento",
                                               "Enfermedades del sistema nervioso",
                                               "Enfermedades del ojo y sus anexos y enfermedades del oído y de la apófisis mastoides",
                                               "Enfermedades del sistema circulatorio",
                                               "Enfermedades del sistema respiratorio",
                                               "Enfermedades del aparato digestivo",
                                               "Enfermedades de la piel y el tejido subcutáneo",
                                               "Enfermedades del sistema osteomuscular y del tejido conectivo",
                                               "Enfermedades del aparato genitourinario",
                                               "Embarazo, parto y puerperio",
                                               "Ciertas afecciones originadas en el periodo perinatal",
                                               "Malformaciones congénitas, deformidades y anomalías cromosómicas",
                                               "Síntomas, signos y hallazgos anormales clínicos y de laboratorio, no clasificados en otra parte",
                                               "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
                                               "Traumatismos, envenenamientos y algunas otras consecuencias de causa externa",
                                               "Códigos para situaciones especiales",
                                               "Causas externas de morbilidad y de mortalidad",
                                               "Causas externas de morbilidad y de mortalidad",
                                               "Causas externas de morbilidad y de mortalidad",
                                               "Causas externas de morbilidad y de mortalidad",
                                               "Factores que influyen en el estado de salud y contacto con los servicios de salud"))
cache('causas_agregadas')
                               
agr_sex %>% group_by(pad_agr) %>% 
  summarise(n = n()) %>% 
  left_join(causas_agregadas, by = c("pad_agr"="clave")) %>% 
  ungroup %>% 
  group_by(descripcion) %>% 
  summarise(casos = sum(n)) %>% 
  arrange(desc(casos)) %>% 
  data.frame

