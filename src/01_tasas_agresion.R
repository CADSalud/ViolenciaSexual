

library(ProjectTemplate)
load.project()

source("munge/01_envin.R")
load("cache/tab_vic2.RData")
load("cache/input.l.Rdata")

fls <- list.files("cache/")[str_detect(list.files("cache/"), "^tab_sec_")]
for(file.nom in fls){
  print(file.nom)
  load( paste0("cache/", file.nom))
}

# input.l <- list()


# TASA DE AGRESIÓN ----
base_endireh <- tab_sec_x %>% 
  group_by(SEXO) %>% 
  summarise(base_mujeres = sum(FAC_MUJ))
input.l$base_endireh <- base_endireh


# 2016 (endireh)

#  Preguntas ----

# P6_6_15: Durante su vida de estudiante, ¿alguna persona de las escuelas a las que asistió a estudiar, (como maestra o maestro, director, prefecto, compañero de clase o alumno de la escuela u otro trabajador o persona de la escuela) la han obligado a tener relaciones sexuales en contra de su voluntad?
# P7_9_13:	Dígame si en alguno de sus trabajos, ¿Alguna o algunas personas que trabajaban con usted	la han obligado a tener relaciones sexuales en contra de su voluntad?
# P8_1_13:	¿Alguna vez la han obligado a tener relaciones sexuales en contra de su voluntad?
# P10_1_3:	Durante el último año, de octubre de 2015 a la fecha, ¿alguna o algunas personas de su familia (no incluya a su pareja o esposo) la han obligado a tener relaciones sexuales en contra de su voluntad
# P12_17_5: ¿Alguna de sus parejas o esposo(s) anteriores, ¿durante su relación o después de separarse la agredió sexualmente (intentó obligarla o la obligó a tener relaciones sexuales por la fuerza o con amenazas)?
# P11_12_5:	Durante su infancia (hasta antes de cumplir 15 años), ¿dígame si vivió o le ocurrieron algunas de las situaciones que se mencionan en la tarjeta? ¿La obligaron a tener relaciones sexuales bajo amenazas o usando la fuerza?



# Tabla con preguntas ----
names(tab_sec_vi)
tab_union <- tab_sec_vi %>% 
  dplyr::select(ID_VIV:FAC_MUJ, edad_num, P6_6_15) %>% # nrow 111,256
  left_join(tab_sec_vii %>% 
              dplyr::select(ID_VIV:FAC_MUJ, edad_num, P7_9_13)) %>% 
  left_join(tab_sec_viii %>% 
              dplyr::select(ID_VIV:FAC_MUJ, edad_num, P8_1_13)) %>% 
  left_join(tab_sec_x %>% 
              dplyr::select(ID_VIV:FAC_MUJ, edad_num, P10_1_3)) %>% 
  left_join(tab_sec_xii %>% 
              dplyr::select(ID_VIV:FAC_MUJ, edad_num, P12_17_5)) %>% 
  left_join(tab_sec_xi %>% 
              dplyr::select(ID_VIV:FAC_MUJ, edad_num, P11_12_5)) %>% 
  mutate(P10_1_3 = ifelse(parse_number(P10_1_3) %in% 1:3, 1, 2),
         edad_gpo = cut(edad_num, include.lowest = T,
                        breaks = c(15, 19, 100)) )
dim(tab_union)
tab_union %>% data.frame() %>% head()
cache("tab_union")



tab <- tab_union %>%
  filter(edad_gpo == "[15,19]") %>% 
  gather(pregunta, resp, P6_6_15:P11_12_5) %>%
  mutate(pregunta = fct_recode(pregunta,
                               `estudiante` = "P6_6_15",
                               `trabajo` = "P7_9_13",
                               `comunidad` = "P8_1_13",
                               `familia` = "P10_1_3",
                               `pareja` = "P12_17_5",
                               `infancia` = "P11_12_5")) %>% 
  group_by(pregunta, resp) %>% 
  summarise(nfac = sum(FAC_MUJ),
            n = n()) %>% 
  group_by(pregunta, resp) %>% 
  mutate(nfac_sin = sum((!is.na(resp))*nfac, na.rm = T)) %>% 
  group_by(pregunta) %>% 
  mutate(prop = 100*nfac/sum(nfac), 
         prop_sin = 100*nfac/sum(nfac_sin),
         base = sum(nfac),
         base_sin = sum(nfac_sin)) %>% 
  ungroup
tab

gg <- tab %>% 
  filter(resp == 1,
         pregunta != "infancia") %>% 
  ggplot(aes(x = pregunta, 
             y = prop_sin) )+ 
  geom_bar(stat = "identity", position = "stack", 
           fill = "blue", alpha = .5) + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Ambiente") + 
  ggtitle("Proporción de mujeres agredidas por ambiente.", 
          "Mujeres entre 15 y 19 años.")
gg 
input.l$gg_agr_tard_amb_endireh <- gg


tab <- tab_union %>%
  filter(edad_gpo == "[15,19]") %>%
  mutate(P11_12_5 = factor(P11_12_5, 
                           c(1,2,8), 
                           c("Sí", "No", "No recuerda"))) %>% 
  group_by(P11_12_5) %>% 
  summarise(nfac = sum(FAC_MUJ),
            n = n()) %>% 
  ungroup %>% 
  mutate(prop = 100*nfac/sum(nfac), 
         base = sum(nfac)) %>% 
  ungroup 
tab
input.l$tab_agr_temp_endireh <- tab

gg <- tab %>% 
  filter(P11_12_5 != "No") %>% 
  ggplot(aes(x = P11_12_5, 
             y = prop) )+ 
  geom_bar(stat = "identity", position = "stack", width = .6,
           fill = "blue", alpha = .5) + 
  geom_label(aes(y = prop + .15, 
                 label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Respuesta") + 
  ggtitle("Proporción de mujeres agredidas\nantes de cumplir 15 años.",
          "Mujeres de entre 15 y 19 años.")
gg 
input.l$gg_agr_temp_endireh <- gg

tab <- tab_union %>%
  dplyr::select(ID_MUJ, ID_VIV, UPM, FAC_MUJ, FAC_VIV,
                edad_gpo,
                P6_6_15:P11_12_5) %>% 
  gather(pregunta, resp, P6_6_15:P11_12_5, -P12_17_5) %>%
  mutate(pregunta = fct_recode(pregunta,
                               `estudiante` = "P6_6_15",
                               `trabajo` = "P7_9_13",
                               `comunidad` = "P8_1_13",
                               `familia` = "P10_1_3",
                               `pareja` = "P12_17_5",
                               `infancia` = "P11_12_5"),
         value = ifelse(resp != 1 | is.na(resp), F, T)) %>% 
  group_by(ID_MUJ, ID_VIV, UPM, FAC_MUJ) %>% 
  summarise(acum = sum(value) > 0) %>%
  ungroup %>% 
  mutate(resp = FAC_MUJ*as.numeric(acum)) %>% 
  summarise(n = sum(resp),
            base = sum(FAC_MUJ), 
            p = 100*n/base)
tab
input.l$tab_agr_endireh <- tab


tab <- tab_union %>%
  filter(edad_gpo == "[15,19]") %>%
  dplyr::select(ID_MUJ, ID_VIV, UPM, FAC_MUJ, FAC_VIV,
                edad_gpo,
                P6_6_15:P11_12_5) %>% 
  gather(pregunta, resp, P6_6_15:P11_12_5, -P12_17_5) %>%
  mutate(pregunta = fct_recode(pregunta,
                               `estudiante` = "P6_6_15",
                               `trabajo` = "P7_9_13",
                               `comunidad` = "P8_1_13",
                               `familia` = "P10_1_3",
                               `pareja` = "P12_17_5",
                               `infancia` = "P11_12_5"),
         value = ifelse(resp != 1 | is.na(resp), F, T)) %>% 
  group_by(ID_MUJ, ID_VIV, UPM, FAC_MUJ) %>% 
  summarise(acum = sum(value) > 0) %>%
  ungroup %>% 
  mutate(resp = FAC_MUJ*as.numeric(acum)) %>% 
  summarise(n = sum(resp),
            base = sum(FAC_MUJ), 
            p = 100*n/base)
tab
input.l$tab_agr_tard_endireh <- tab

# cache("input.l")


# 2016 (envipe) ----

# 18 años
tab_envipe <- tab_vic2 %>% 
  filter(situacion == "14") %>% 
  mutate(SEXO = factor(SEXO, 1:2, c("H", "M")))

input.l$base_sexo_envipe <- tab_envipe %>% 
  group_by(SEXO) %>% 
  summarise(base = sum( parse_number(FAC_ELE)))


# tasa general
tab <- tab_envipe %>% 
  group_by(ocurrió) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE))) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac), 
         base = sum(nfac)) 
tab
input.l$tab_agr_gral_envipe <- tab

# tasa por sexo
tab <- tab_envipe %>% 
  group_by(ocurrió, SEXO) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE))) %>% 
  group_by(ocurrió) %>% 
  mutate(prop_ocu = nfac/sum(nfac)) %>% 
  group_by(SEXO) %>% 
  mutate(prop_sex = nfac/sum(nfac)) %>% 
  ungroup
tab
input.l$tab_agr_sexo_envipe <- tab

gg <- tab %>% 
  filter(ocurrió == 1) %>% 
  ggplot(aes(x = SEXO, 
             y = prop_ocu) )+ 
  geom_bar(stat = "identity", position = "stack", width = .5,
           fill = "blue", alpha = .5) + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Sexo") + 
  ggtitle("Proporción de personas agredidas \npor sexo.",
          "Hombres y mujeres mayores de 18 años.")
gg
input.l$gg_agr_sexo_envipe <- gg


tab_envipe %>% 
  filter(ocurrió == 1) %>% 
  group_by(num_veces) %>% 
  summarise(n = n(), 
            nfac = sum(parse_number(FAC_ELE)))




# 2007 (envin) ----
# numero de mujeres 15 a 24 años
input.l$base_envin <- (tab_mjov2$FAC_PER %>% sum) # 14 millones mujeres entre 15 y 24 años

# algunas vez obligaron
tab <- tab_mjov2 %>% 
  group_by(P9_6) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  ungroup %>% 
  na.omit %>% 
  mutate(prop = 100*nfac/sum(nfac))
tab
input.l$tab_agr_muj_envin <- tab


# varias veces
tab_mjov2 %>% 
  group_by(P9_7) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER))


# edad de primera vez obligaron
tab <- tab_mjov2 %>%
  mutate(edad_gpo = cut(parse_number(P9_9),
                        right = T,
                        breaks = c(0, 9, 14, 19, 100), 
                        include.lowest = T)) %>% 
  group_by(edad_gpo) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  ungroup %>% 
  na.omit %>% 
  mutate(prop = 100*nfac/sum(nfac))
tab
input.l$tab_agr_edad1era_envin <- tab


gg <- ggplot(tab, aes(x = edad_gpo, 
                y = prop)) + 
  geom_bar(stat = "identity", 
           fill = "blue", alpha = .5) + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Grupo de edad") + 
  ggtitle("Distribución por edad de primera agresión.",
          "Mujeres entre 15 y 24 años.")
gg
input.l$gg_agr_edad1era_envin <- gg


# alguna vez embarazo
tab <- tab_mjov2 %>% 
  mutate(edad_gpo = cut(parse_number(P9_9),
                                    right = T,
                                    breaks = c(0, 9, 14, 19, 100), 
                                    include.lowest = T)) %>% 
  group_by(P9_12) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(prop = 100*nfac/sum(nfac))
tab
input.l$tab_agr_emb_envin <- tab

cache("input.l")
