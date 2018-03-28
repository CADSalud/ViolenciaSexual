
library(ProjectTemplate)
load.project()

library(Hmisc)

# endireh
load("cache/tab_sec_xii.RData") 
load("cache/input.l.RData") 

tab_sec_xii %>% 
  filter(edad_num <= 19, edad_num>=10) %>% 
  group_by(SEXO) %>% 
  summarise(sum(FAC_MUJ))


# Edad del primer hijo
tab_eprimerh <- tab_sec_xii %>% 
  mutate(edad_primerh = parse_number(P12_2),
         edad_gpo_primerh = cut( ifelse(edad_primerh > 95, NA, edad_primerh),
                                 include.lowest = T,
                                 breaks = c(1, 9, 14, 19, 95, 97, 100)), 
         edad_gpo_primerh = fct_explicit_na(edad_gpo_primerh, "(NA)")) 
tab_eprimerh$edad_gpo_primerh %>% unique()

tab_eprimerh %>%
  filter(!is.na(edad_primerh),
         edad_num < 20) %>% 
  summarise(sum(FAC_MUJ), 
            n())

tab_eprimerh$FAC_MUJ %>% sum() # 46 millones de mujeres mayores de 15 años
tab <- tab_eprimerh %>%
  filter(edad_num < 20) %>% 
  filter(!is.na(edad_primerh)) %>% 
  group_by(edad_gpo_primerh) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerh)) %>%
  ungroup %>% 
  mutate(prop = nfac/sum(nfac)) 
tab
input.l$tab_gpoedad_primerh_endireh <- tab


gg <- ggplot(tab, aes(x = edad_gpo_primerh, 
                y = prop)) + 
  geom_bar(stat = "identity", position = "stack",
           fill = "red", alpha = .5, 
           width = .6) + 
  # geom_label(aes(y = prop + .05, 
                 # label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Grupos de edad del primer hijo") + 
  ggtitle("Distribución de edad al primer hijo.",
          "Mujeres entre 15 y 19 años")
gg
input.l$gg_gpoedad_primerh_endireh <- gg
ggsave(plot = gg, filename = "graphs/gg_gpoedad_primerh_endireh.png")

  
gg <- tab_eprimerh %>% 
  filter(edad_num < 20) %>% 
  filter(edad_primerh < 99) %>% 
  ggplot(aes(x = "", y = edad_primerh)) + 
  geom_boxplot(fill = "red", alpha = .5) + 
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  ggtitle("Distribución de edad al primer hijo",
          "Mujeres entre 15 y 19 años") + 
  coord_flip() + 
  xlab(NULL) + 
  ylab("Edad en años al primer hijo")
gg
input.l$gg_edad_primerh <- gg


tab_sec_xii %>% 
  dplyr::select(starts_with("FAC_"), 
                edad_primerh = P12_2,
                SEXO, EDAD, edad_num) 



# Edad de la primera relacion ----

tab_eprimerarel <- tab_sec_xii %>%
  filter(edad_num < 20) %>% 
  mutate(edad_primerarel = parse_number(P12_6),
         edad_gpo_primerarel = cut( edad_primerarel,
                                 include.lowest = T,
                                 breaks = c(5, 9, 14, 19, 100)), 
         consentimiento = factor(P12_7, c(1:2, 9), c("sí", "no", "no especificado")))# %>% 
  filter(edad_primerarel < 90,
         !is.na(edad_primerarel))
table(tab_eprimerarel$consentimiento, tab_eprimerarel$P12_7 ) 

tab_eprimerarel %>% 
  filter(edad_num < 20) %>% 
  # group_by(edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ))

tab_eprimerarel %>% 
  filter(edad_num < 20) %>% 
  group_by(edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  write.csv()


tt <- tab_eprimerarel %>% 
  filter(edad_num < 20) %>% 
  group_by(edad_gpo_primerarel, 
           consentimiento) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  # filter(!is.na(edad_gpo_primerarel)) %>% 
  group_by(edad_gpo_primerarel) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup 
tt
input.l$tab_gpoedad_primerarel_endireh <- tt

gg <-
  ggplot(tt, aes(x = edad_gpo_primerarel, 
               y = prop)) + 
  geom_bar(aes(fill = consentimiento),
           alpha = .5,
           stat = "identity", position = "stack", 
           width = .8) + 
  theme(legend.position = "right") +
  ylab("Proporción (%)") + 
  xlab("Edad de primera relación") + 
  ggtitle("Consentimiento por de edad de primera relación", 
          "Mujeres entre 15 y 19 años")
gg
input.l$gg_gpoedad_primerarel_endireh <- gg


# Edad de primer union ----
tab_conyug_i <- tab_sec_xii %>% 
  mutate(edad_conyug = parse_number(P12_14),
         edad_gpo_conyug = cut( edad_conyug,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_15AB), 
         edad_gpo_mujer = cut(edad_num, include.lowest = T, 
                          breaks = c(15, 19, 100))) #%>% 
  filter(edad_conyug < 90, 
         edad_pareja < 90)
tab_conyug_i %>% data.frame() %>% head

dim(tab_conyug_i)
dim(tab_sec_xii)

tab_sec_xii %>% 
  filter(edad_num < 20) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ))

tab_conyug_i %>% 
  filter(!is.na(edad_conyug), 
         edad_num < 20) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ))

gg <- tab_conyug_i %>% 
  filter(!is.na(edad_conyug), 
         edad_conyug < 70) %>%  
  group_by(edad_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  ungroup() %>% 
  mutate(prop = nfac/sum(nfac), 
         total = sum(nfac)) %>% 
  ggplot(aes(x = edad_conyug, y = prop)) + 
  geom_line(color = "gray60") + 
  geom_point(aes(size = n, 
                 alpha = nfac)) + 
  ggtitle("Distribución de edad de primera unión", 
          "Mujeres de 15 a 70 años") + 
  ylab("Proporción") + 
  xlab("Edad primera unión") + 
  guides(size = guide_legend("Casos observados"), 
         alpha = guide_legend("Casos ponderados"))
ggsave(filename = "graphs/distrib_primera_union.png", plot = gg, width = 7, height = 4)


tab_conyug_i %>% 
  filter(edad_num < 90) %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))


tab <- tab_conyug_i %>% 
  filter(edad_num < 20) %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))
tab
input.l$tab_gpoedad_primeraun <- tab

gg <- ggplot(tab, aes(x = edad_gpo_conyug, 
                y  = prop)) + 
  geom_bar(stat = "identity", position = "dodge",
           fill = "red", alpha = .5) + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ggtitle("Distribución de edad de primera unión", 
          "Mujeres entre 15 y 19 años") + 
  ylab("Proporción (%)") + 
  xlab("Edad de la primera unión")
gg
input.l$gg_gpoedad_primeraun <- gg


tt <- tab_conyug_i %>% 
  filter(edad_num < 20) %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup
tt

input.l$tab_prop_edadparejaunion_endireh <- tt

gg <- tt %>% 
  filter(edad_pareja < 75) %>% 
  ggplot(aes(x = edad_pareja, 
             y = prop, 
             color = edad_gpo_conyug)) + 
  geom_line(size = 2) + 
  scale_color_brewer(palette = "Blues") +
  geom_vline(xintercept = 18) + 
  ylab("Proporción (%)") + 
  xlab("Edad de la pareja") + 
  guides(color = guide_legend("Grupo de edad\nmujer")) + 
  ggtitle("Probabilidad de la edad de la pareja.",
          "Mujeres entre 15 y 19 años")
gg
input.l$gg_prop_edadparejaunion_endireh <- gg


wgtquant_fun <- function(sub){
  vecs <- wtd.quantile(sub$edad_pareja, weights = sub$FAC_MUJ)
  tibble(edad_pareja = vecs,
         probs = str_trim(names(vecs)) ) 
}

gg <- tab_conyug_i %>% 
  filter(edad_num < 20) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  group_by(edad_gpo_conyug) %>% 
  do(wgtquant_fun(sub = .)) %>% 
  ungroup %>% 
  spread(probs, edad_pareja) %>% 
  ggplot(aes(x = edad_gpo_conyug)) + 
  geom_boxplot(aes(ymin = `0%`, 
                   lower = `25%`, 
                   middle = `50%`, 
                   upper = `75%`, 
                   ymax = `100%`, 
                   fill = edad_gpo_conyug),
               stat = "identity", 
               alpha = .5) + 
  geom_hline(yintercept = 18, color = "gray40", 
             linetype = 3, 
             size = 1) +
  geom_hline(yintercept = 18, color = "gray40", 
             linetype = 1, 
             size = .5) + 
  annotate("text", label = "mayoría de edad", 
           color = "gray40",
           x = 0.5, y = 20, 
           hjust =0) +
  xlab("Grupo de edad") + 
  ylab("Edad de la pareja") + 
  theme(legend.position = "none") +
  ggtitle("Distribución de la edad de la pareja.",
          "Grupo de edad de la mujer en la primera union.") + 
  coord_flip()
gg
input.l$gg_box_edadparejaunion_endireh <- gg



gg <- tab_conyug_i %>% 
  filter(edad_num < 20) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ggplot(aes(x = edad_gpo_conyug, y = edad_pareja)) + 
  geom_hline(yintercept = 18, color = "gray40", 
             linetype = 3, 
             size = 1) +
  geom_hline(yintercept = 18, color = "gray40", 
             linetype = 1, 
             size = .5) +
  geom_boxplot(aes(fill = edad_gpo_conyug), 
               alpha = .5) + 
  annotate("text", label = "mayoría de edad", 
           color = "gray40",
           x = 0.5, y = 20, 
           hjust =0) +
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  xlab("Grupo de edad") + 
  ylab("Edad de la pareja") + 
  theme(legend.position = "none") +
  ggtitle("Edad de la pareja por\nedad de la mujer en la primera union.",
          "Mujeres entre 15 y 19 años") +  
  facet_wrap(~edad_gpo_mujer) +
  coord_flip()
gg
input.l$gg_box_edadparejaunion_gpomuj_endireh <- gg

# edad de primera union ENDIREH
tab <- tab_conyug_i %>% 
  filter(edad_num < 20) %>% 
  group_by(CVE_ENT, NOM_ENT, edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) 
tab %>% data.frame()
input.l$tab_entprimeraunion_gpomuj_endireh <- tab


# edo<-readShapeSpatial("data/mex_edos/Mex_Edos")
# edo@data$id <- rownames(edo@data)
# edo_df<- edo %>%
#   fortify() %>%
#   mutate(id=as.numeric(id)+1)
# 
# edo_subdiag <- left_join(edo_df, tab, by = 'id') 
# 
# gg <- ggplot(data = edo_subdiag, aes(long, lat, group=group)) + 
#   geom_polygon( aes(fill = prom, group = group),
#                 color='black', size = .4) + 
#   coord_fixed() +
#   theme(axis.text = element_blank(), 
#         axis.title = element_blank()
#   ) + 
#   scale_fill_continuous(low = 'white', high = '#132B43') + 
#   guides(fill = guide_legend(title = "Time to Job\nPromedio")) +
#   theme(legend.position = 'top')


# Razones de union actual ----
tab_razon <- tab_sec_xii %>%
  filter(edad_num < 20) %>%
  dplyr::select(ID_VIV:FAC_MUJ, P12_9, P12_10, P12_11) %>% 
  mutate(edad_union = parse_number(P12_9),
         edad_gpo_union = cut( edad_union,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 95, 100)), 
         edad_pareja = parse_number(P12_10), 
         razones = factor(P12_11, 1:7, 
                          c("embarazo y obligaron", 
                            "embarazo y decision mutua", 
                            "robaron en contra de voluntad", 
                            "arreglo a cambio de dinero, etc", 
                            "salir de casa", 
                            "decisión mutua", 
                            "otro"))) #%>% 
  # filter(edad_union < 90)
tab_razon


tab_razon %>% 
  group_by(is.na(edad_gpo_union)) %>% 
  summarise(base = sum(FAC_MUJ))

tab_razon %>% 
  group_by(edad_gpo_union) %>% 
  summarise(base = sum(FAC_MUJ), 
            n = n()) %>% 
  write.csv()


tab <- tab_razon %>% 
  group_by(edad_gpo_union, razones) %>% 
  summarise(nfac = sum(FAC_MUJ),
            n = n()) %>% 
  left_join(tab_razon %>% 
              group_by(edad_gpo_union) %>% 
              summarise(base = sum(FAC_MUJ)), 
            by = "edad_gpo_union") %>% 
  mutate(prop = nfac/base) %>% 
  ungroup() %>% 
  na.omit()
tab
input.l$tab_razones_endireh <- tab

gg <- tab %>% 
  ggplot(aes(x = edad_gpo_union, y = prop)) + 
  geom_bar(aes(fill = razones), 
           stat = "identity", 
           position = "dodge", 
           alpha = .7, 
           width = .6) +
  ggtitle("Razones de unión",
          "Mujeres de 15 a 19 años")
gg
input.l$gg_razones_endireh <- gg

cache("input.l")



# Enadid ----
load("cache/tab_muj1.RData")
tab_muj1$EDAD_1AG %>% summary()
tab_muj1$edad_num %>% summary()
tab_muj1$FAC_PER %>% sum() # 2014: 35,204,085 de 15 a 54 años

# Embarazo alguna vez
tab_muj1 %>% 
  group_by(EDAD_1AG, P5_6) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  ungroup

tab_muj1 %>% 
  group_by(edad_num, algunavez_emb) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(edad_num) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup %>% 
  filter(algunavez_emb) %>%
  ggplot(aes(x = edad_num, y = prop, 
             group = algunavez_emb)) + 
  # geom_point() + 
  geom_line(size = 1, color = "blue") + 
  scale_y_continuous(breaks = seq(0, 1, .05)) + 
  scale_x_continuous(breaks = seq(15, 54, 2)) + 
  ggtitle("Proporción de mujeres alguna vez embarazadas.") +
  ylab("Proporción") + 
  xlab("Edad de mujer") + 
  labs(caption = "Fuente: ENADID 2014")
  


# Abandono escolar y causas
tab_muj1 %>% 
  group_by(P5_3) %>% 
  tally() %>% 
  ungroup %>% 
  mutate(prop = n/sum(n))

tab_muj1 %>% 
  # filter(P5_3 == 1) %>% 
  group_by(EDAD_1AG, P5_3) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(EDAD_1AG) %>% 
  mutate(prop = nfac/sum(nfac))

tab_muj1 %>% 
  group_by(P5_3, P5_4_rec) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(P5_3) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  filter(P5_3 == 1) %>% 
  ggplot(aes(x = fct_reorder(P5_4_rec, prop),
             y = prop)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  ggtitle("Distribución de razones de abandono de escuela.", 
          "Mujeres de 15 a 54 años") +
  xlab(NULL) + 
  ylab("Proporción") + 
  labs(caption = "Fuente: ENADID 2014")

gg <- tab_muj1 %>% 
  filter(EDAD_1AG == "15 a 19") %>% 
  group_by(P5_3, P5_4_rec) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(P5_3) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  filter(P5_3 == 1) %>% 
  ggplot(aes(x = fct_reorder(P5_4_rec, prop),
             y = prop)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  ggtitle("Distribución de razones de abandono de escuela.", 
          "Mujeres de 15 a 19 años") +
  xlab(NULL) + 
  ylab("Proporción") + 
  labs(caption = "Fuente: ENADID 2014")
gg
input.l$gg_aband_enadid <- gg

tab_muj1 %>% 
  filter(P5_3 == 1) %>% 
  group_by(EDAD_1AG, P5_4_rec) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(EDAD_1AG) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup %>% 
  ggplot(aes(x = fct_reorder(P5_4_rec, prop),
             y = prop, 
             group = EDAD_1AG)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~EDAD_1AG, scales = "free", nrow = 1) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Distribución de razones de abandono de escuela.", 
          "Grupos de edad quinquenales") +
  xlab(NULL) + 
  ylab("Proporción") + 
  labs(caption = "Fuente: ENADID 2014") + 
  theme(axis.text.x = element_text(hjust = 1))

tab_muj1 %>% 
  filter(P5_3 == 1) %>% 
  group_by(EDAD_1AG, P5_4_rec) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(EDAD_1AG) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup %>% 
  ggplot(aes(x = EDAD_1AG,
             y = prop, 
             fill = fct_reorder(P5_4_rec, prop))) + 
  geom_bar(stat = "identity")


# Aborto
tab_muj1 %>% 
  filter(!is.na(P5_22)) %>% 
  group_by(EDAD_1AG, P5_22) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_PER)) %>% 
  group_by(EDAD_1AG) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  filter(P5_22 == 1) %>% 
  ggplot(aes(x = EDAD_1AG, 
             y = prop)) + 
  geom_linerange(aes(ymin = 0, ymax = prop)) + 
  geom_point() + 
  ggtitle("Proporción de mujeres algún aborto.", 
          "Mujeres alguna vez embarazadas")


# Edad de casadas endireh 5-19
tab_sec_xii$P2_16 %>% table()

tab <- tab_sec_xii %>% 
  mutate(edo_civil = factor(P2_16, c(1:6, 9), 
                            c("union libre", "separada", "divorciada",
                              "viuda", "casada", "soltera",
                              "no especificado")) ) %>% 
  filter(edad_num <= 19) %>% 
  group_by(edo_civil) %>% 
  summarise(nfac = sum(FAC_MUJ), 
            n = n()) %>% 
  ungroup() %>% 
  mutate(prop = 100*nfac/sum(nfac))
tab
input.l$tab_edocivi_endireh <- tab
