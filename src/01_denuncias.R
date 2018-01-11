
library(ProjectTemplate)
load.project()

load("cache/dat.denuncias.RData")
load("cache/input.l.RData")


# códigos
cods.viosex <- c('1020', '1030', '2020',  '5010', 
                 '3010', '3020', '3030', '3040', '3050', '3060', '3070', '3080',
                 '6011', '6012', '6013', '6014', '6015', '6021', '6030')

labs.viosex <- c('feminicidio', 'aborto', 'tráfico de menores', 'violencia familiar',
                 'abuso sexual','acoso sexual','hostigamiento','violacion',
                 'violacion equiparada','estupro','incesto', 'otros contra lib. sexual',
                 'corrupcion de menores e inc.', 'prostitución de menores e inc.', 
                 'pornografía infantil', 'lenocinio', 'turismo sexual', 
                 'trata con fines exp. sexual', 'violencia de género')


# Violencia sexual ----
df.viosex <- dat.denuncias %>% 
  filter(DELI_FC %in% cods.viosex)
df.viosex

df.viosex$DESC_TIP2 <- df.viosex$DESC_TIP %>% as.vector() #%>% str_trim()

df.viosex$desc_delito %>% unique
df.viosex$edad %>% unique


df.viosex$DESC_TIP %>% unique()
df.viosex$DELI_FC %>% unique() %>% paste("'", ., "'", collapse = ",", sep = "")
df.viosex$edad %>% levels() %>% paste("'", ., "'", collapse = ",", sep = "")

levels(df.viosex$edad) <- c('0 a 4',
                            '10 a 14','15 a 19','20 a 24','25 a 29',
                            '30 a 34','35 a 39','40 a 44','45 a 49',
                            '5 a 9','50 a 54','55 a 59',
                            '60  y más','No identificado')

edad.orden <- c('0 a 4','5 a 9',
                '10 a 14','15 a 19','20 a 24','25 a 29',
                '30 a 34','35 a 39','40 a 44','45 a 49',
                '50 a 54','55 a 59',
                '60  y más','No identificado')

tab.viosex <- df.viosex %>% 
  mutate(desc_delito = factor(DELI_FC, levels = cods.viosex, labels = labs.viosex),
         edad_orden = factor(edad, levels = edad.orden)) 

tab <- tab.viosex %>% 
  filter(sexo_rec == "Muj") %>% 
  group_by(edad_orden, desc_delito) %>% 
  summarise(n_tt = sum(TT_VICT, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(edad_orden) %>% 
  mutate(prop_edad = 100*n_tt/sum(n_tt)) %>% 
  group_by(desc_delito) %>% 
  mutate(prop_delito = 100*n_tt/sum(n_tt)) %>% 
  ungroup()
tab

input.l$tab_avp_vic <- tab

gg <- tab %>% 
  ggplot(aes(x = edad_orden, 
             y = n_tt)) + 
  geom_bar(stat = "identity", 
           fill = "blue", alpha = .6) + 
  facet_wrap(~desc_delito, scales = "free", ncol = 4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Mujeres víctimas por clasificación de delito y edad", 
          "Aver. previas iniciadas y carpetas de inv. abiertas 2015") +
  ylab("Número de víctimas registradas \n averiguaciones previas iniciadas") + 
  xlab("Grupo de edad")
gg
input.l$gg_avp_vic <- gg



tab <- tab.viosex %>% 
  group_by(sexo_rec, edad_orden, desc_delito) %>% 
  summarise(n_tt = sum(TT_VICT, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(sexo_rec, edad_orden) %>%
  mutate(prop_edad = 100*n_tt/sum(n_tt)) %>%
  group_by(desc_delito) %>%
  mutate(prop_delito = 100*n_tt/sum(n_tt)) %>%
  ungroup() 
tab

input.l$tab_avp_sexo_vic <- tab

cache("input.l")
