
library(ProjectTemplate)
load.project()

library(foreign)

load("cache/input.l.RData")


dat.vic <- read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Bases_datos/VICTIMAS.DBF") %>% 
  as.tibble() %>% 
  left_join(read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Catalogos/DELI_FC.DBF"), 
            by = "DELI_FC") %>% 
  left_join(read.dbf("data/censo_trib_superior/AP_iniciadas_CI_abiertas_cnpje2016_dbf/Catalogos/RAN_EDAD.DBF"),
            by = "RAN_EDAD") %>% 
  mutate(tipo_del = parse_number(str_sub(DELI_FC, 1, 2)),
         desc_delito = factor(str_trim(DESC_TIP)), 
         edad = factor(str_trim( str_replace_all(DESCRIP, "a\xa4os", ""))),
         sexo_rec = factor(SEXO, levels = c(1,2), labels = c("Hom", "Muj")))
dat.vic %>% data.frame %>% head
dat.vic %>% dim
dat.vic %>% names

dat.vic$tipo_del %>% summary
dat.vic$desc_delito %>% unique()
dat.vic$edad %>% unique()

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
df.viosex <- dat.vic %>% 
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

gg <- tab.viosex %>% 
  filter(sexo_rec == "Muj") %>% 
  group_by(edad_orden, desc_delito) %>% 
  summarise(n_tt = sum(TT_VICT, na.rm = T)) %>% 
  ungroup() %>% 
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

cache("input.l")


