
library(ProjectTemplate)
load.project()

# endireh
load("cache/tab_sec_xii.RData") 
load("cache/input.l.RData") 


# Edad del primer hijo
tab_eprimerh <- tab_sec_xii %>% 
  mutate(edad_primerh = parse_number(P12_2),
         edad_gpo_primerh = cut( edad_primerh,
                                 include.lowest = T,
                                 breaks = c(1, 9, 14, 19, 100)))

tab_eprimerh$FAC_MUJ %>% sum() # 46 millones de mujeres mayores de 15 años
tab <- tab_eprimerh %>% 
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
  geom_label(aes(y = prop + .05, 
                 label = format(nfac, big.mark = ","))) + 
  ylab("Proporción (%)") + 
  xlab("Grupos de edad") + 
  ggtitle("Distribución de edad al primer hijo.")
gg
input.l$gg_gpoedad_primerh_endireh <- gg

  
gg <- tab_eprimerh %>% 
  filter(edad_primerh < 99) %>% 
  ggplot(aes(x = "", y = edad_primerh)) + 
  geom_boxplot(fill = "red", alpha = .5) + 
  scale_y_continuous(breaks = seq(0, 110, 5)) +
  ggtitle("Distribución de edad al primer hijo") + 
  coord_flip() + 
  xlab(NULL) + 
  ylab("Edad en años")
gg
input.l$gg_edad_primerh <- gg


tab_sec_xii %>% 
  dplyr::select(starts_with("FAC_"), 
                edad_primerh = P12_2,
                SEXO, EDAD, edad_num) 



# Edad de la primera relacion ----

tab_eprimerarel <- tab_sec_xii %>% 
  mutate(edad_primerarel = parse_number(P12_6),
         edad_gpo_primerarel = cut( edad_primerarel,
                                 include.lowest = T,
                                 breaks = c(5, 9, 14, 19, 100)), 
         consentimiento = factor(P12_7, 1:2, c("sí", "no")))

tt.0 <- tab_eprimerarel %>% 
  group_by(edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac), 
         consentimiento = "total")

tt <- tab_eprimerarel %>% 
  group_by(consentimiento, 
           edad_gpo_primerarel) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_primerarel)) %>% 
  group_by(consentimiento) %>% 
  mutate(prop = nfac/sum(nfac)) %>% 
  ungroup %>% 
  bind_rows(tt.0)
tt

gg <-
  ggplot(tt, aes(x = edad_gpo_primerarel, 
               y = prop)) + 
  geom_bar(aes(fill = consentimiento),
           alpha = .5,
           stat = "identity", position = "dodge", 
           width = .8) + 
  geom_label(aes(y = prop +.03, 
                 label = format(nfac, big.mark = ","))) + 
  facet_wrap(~consentimiento) + 
  theme(legend.position = "none") +
  ylab("Proporción (%)") + 
  xlab("Grupo de edad") + 
  ggtitle("Distribución de edad de primera relación", 
          "Consentimiento de la relación")
gg
input.l$gg_gpoedad_primerarel_endireh <- gg


# Edad de primer union ----
tab_conyug_i <- tab_sec_xii %>% 
  mutate(edad_conyug = parse_number(P12_14),
         edad_gpo_conyug = cut( edad_conyug,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_15AB))
tab_conyug_i

tab <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  filter(!is.na(edad_gpo_conyug)) %>% 
  ungroup %>% 
  mutate(prop = nfac/sum(nfac))
tab

gg <- ggplot(tab, aes(x = edad_gpo_conyug, 
                y  = prop)) + 
  geom_bar(stat = "identity", position = "dodge",
           fill = "red", alpha = .5) + 
  geom_label(aes(label = format(nfac, big.mark = ","))) + 
  ggtitle("Distribución de edad de primera unión") + 
  ylab("Proporción (%)") + 
  xlab("Grupo de edad")
gg
input.l$gg_gpoedad_primeraun <- gg


tt <- tab_conyug_i %>% 
  group_by(edad_gpo_conyug, edad_pareja) %>% 
  summarise(n = n(), 
            nfac = sum(FAC_MUJ)) %>% 
  group_by(edad_gpo_conyug) %>% 
  mutate(prop = 100*nfac/sum(nfac)) %>% 
  ungroup
tt

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
          "Grupo de edad de la mujer en la primera union.")
gg
input.l$gg_prop_edadparejaunion_endireh <- gg


gg <- tab_conyug_i %>% 
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
  ggtitle("Distribución de la edad de la pareja.",
          "Grupo de edad de la mujer en la primera union.") + 
  coord_flip()
gg
input.l$gg_box_edadparejaunion_endireh <- gg


# Razones de union actual ----
tab_razon <- tab_sec_xii %>%
  dplyr::select(ID_VIV:FAC_MUJ, P12_9, P12_10, P12_11) %>% 
  mutate(edad_union = parse_number(P12_9),
         edad_gpo_union = cut( edad_union,
                                include.lowest = T,
                                breaks = c(5, 9, 14, 19, 100)), 
         edad_pareja = parse_number(P12_10), 
         razones = factor(P12_11, 1:7, 
                          c("emabarazo y obligaron", 
                            "emabarazo y decision mutua", 
                            "robaron en contra de voluntad", 
                            "arreglo a cambio de dinero, etc", 
                            "salir de casa", 
                            "decisión mutua", 
                            "otro")))
tab_razon %>% data.frame() %>% head


tab_razon %>% 
  group_by(edad_gpo_union) %>% 
  summarise(base = sum(FAC_MUJ))

gg <- tab_razon %>% 
  group_by(edad_gpo_union, razones) %>% 
  summarise(nfac = sum(FAC_MUJ),
            n = n()) %>% 
  left_join(tab_razon %>% 
              group_by(edad_gpo_union) %>% 
              summarise(base = sum(FAC_MUJ)), 
            by = "edad_gpo_union") %>% 
  mutate(prop = 100*nfac/base) %>% 
  na.omit() %>% 
  ggplot(aes(x = edad_gpo_union, y = prop)) + 
  geom_bar(aes(fill = razones), 
           stat = "identity", 
           position = "stack", 
           alpha = .7, 
           width = .6)  + 
  geom_label(aes(y = prop - .1 , 
                 label = format(nfac, big.mark = ",")), 
             alpha = .5) + 
  facet_wrap(~razones, scales = "free_y", ncol = 4) + 
  theme(legend.position = "none")
gg
input.l$gg_razones_endireh <- gg

cache("input.l")



# Enadid

tab_muj1$EDAD_1AG %>% summary()
tab_muj1$edad_num %>% summary()
tab_muj1$FAC_PER %>% sum() # 2014: 35,204,085 de 15 a 54 años

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
  coord_flip()

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
