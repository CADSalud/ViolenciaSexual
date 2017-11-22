library(ProjectTemplate)
load.project()

library(readxl)

# Población estimada de 1990 a 2009 (hombres)
pob_1990_2009_hombres <- read_excel('data/CONAPO/RepublicaMexicana_est_1990_2010.xlsx',
                                    sheet = "Población enero",
                                    col_names = FALSE,
                                    range = "A6:U115") %>% 
  mutate(sexo = "Hombres")

# Población estimada de 1990 a 2009 (mujeres)
pob_1990_2009_mujeres <- read_excel('data/CONAPO/RepublicaMexicana_est_1990_2010.xlsx',
                                    sheet = "Población enero",
                                    col_names = FALSE,
                                    range = "A120:U229") %>% 
  mutate(sexo = "Mujeres")

# Población estimada de 1990 a 2009
pob_1990_2009 <- bind_rows(pob_1990_2009_hombres,pob_1990_2009_mujeres)
names(pob_1990_2009) <- c('edad',paste('X',1990:2009,sep=''),'sexo')


# Población proyectada de 2010 a 2051 (hombres)
pob_2010_2051_hombres <- read_excel('data/CONAPO/RepublicaMexicana_pry_2010_2030.xlsx',
                                    sheet = "Población enero",
                                    col_names = FALSE,
                                    range = "A6:AQ115") %>% 
  mutate(sexo = "Hombres")

# Población proyectada de 2010 a 2051 (mujeres)
pob_2010_2051_mujeres <- read_excel('data/CONAPO/RepublicaMexicana_pry_2010_2030.xlsx',
                                    sheet = "Población enero",
                                    col_names = FALSE,
                                    range = "A120:AQ229") %>% 
  mutate(sexo = "Mujeres")

# Población proyectada de 2010 a 2051
pob_2010_2051 <- bind_rows(pob_2010_2051_hombres,pob_2010_2051_mujeres)
names(pob_2010_2051) <- c('edad',paste('X',2010:2051,sep=''),'sexo')


# Población estimada + proyectada de 1990 a 2051
pob_1990_2051 <- bind_cols(pob_1990_2009 %>% select(-sexo),
                           pob_2010_2051 %>% select(-edad))

# Población estimada + proyectada de 1990 a 2051 de 10 a 19
pob_1990_2051_adoles <- pob_1990_2051 %>% 
  filter(edad>9 & edad<20)

poblacion <- pob_1990_2051_adoles %>% 
  gather(key = año,value = pob, -edad, -sexo) %>% 
  mutate(anio = substr(año, start = 2, stop = 5)) %>% 
  select(anio,edad,sexo,pob)

cache('poblacion')

poblacion %>% 
  filter(edad>9 & edad<15) %>% 
  ggplot(aes(x=anio, y = pob, fill = sexo)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  theme(axis.text.x  = element_text(angle = 90))

poblacion %>% 
  filter(edad>9 & edad<15,
         anio == "2012") %>% 
  group_by(sexo) %>% 
  summarise(total = sum(pob))
