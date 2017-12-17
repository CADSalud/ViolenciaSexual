
library(ProjectTemplate)
load.project()



# datos 
load("cache/tab_union.RData")
tab_union %>% data.frame() %>% head
tab_union$P11_12_5 %>% summary()

tab_union


tab_union %>% 
  group_by(P11_12_5) %>% 
  summarise(npond = sum(FAC_MUJ)) %>% 
  ungroup() %>% 
  mutate(porc = npond/sum(npond))



m.boot <- 100

# BROOM ----

porc_fun <- function(sub){
  sub  %>% 
    group_by(P11_12_5) %>% 
    summarise(npond = sum(FAC_MUJ)) %>% 
    ungroup() %>% 
    mutate(porc = npond/sum(npond))
}

boot.porc <- tab_union %>% 
  bootstrap(m = m.boot) %>% 
  do( porc_fun(sub = .) )
boot.porc

porc.broom <- boot.porc %>% 
  ungroup %>% 
  filter(P11_12_5 == 1) 

porc.broom %>% 
  ggplot(aes(x = porc)) + 
  geom_histogram(bins = 10)

tab.res <- tibble( tipo = "broom", 
        prom = mean(porc.broom$porc),
        median = median(porc.broom$porc),
        q975 = quantile(porc.broom$porc, .975),
        q025 = quantile(porc.broom$porc, .025))
tab.res



# SURVEY ----
library(survey)
# 0. Ejemplo -----
# # EJEMPLO: (https://rpubs.com/jcms2665/MAHSDU)
# # Diseño:
# # MAS:
# boot <- svydesign(id = ~1, weights = ~1, data = SD, nest = TRUE)
# # Conglomerados:
# boot <- svydesign(id = ~UPM, weights = ~1, data = SD, nest = TRUE)
# # Estratificado:
# boot <- svydesign(id = ~1, strata = ~EST_D, weights = ~FAC,  data = SD, nest = TRUE)
# # Est/Cong:
# boot <- svydesign(id = ~UPM, strata = ~EST_D, weights = ~FAC,  data = SD, nest = TRUE)
# # Diseño
# boot_design <- as.svrepdesign(boot, type = "bootstrap", replicates = 100)
# # Media de horas ocupadas
# svy <- round(svymean(~HRSOCUP, boot_design, deff = TRUE), 2)
# # Coeficiente de variación de estimador
# cv <- round(data.frame(cv(svy)*100), digits = 2)
# # Todo junto
# dat <- data.frame(svy,cv)

# 1. Población de referencia ----
tab_pob_ref <- tab_union %>% 
  dplyr::select(ID_MUJ, ID_VIV, 
                UPM, FAC_MUJ, 
                edad_num, edad_gpo, P11_12_5) %>% 
  mutate(resp_si = P11_12_5 == 1)
tab_pob_ref

# 2. Bootstrap -----
boot <- svydesign(id = ~1, weights = ~FAC_MUJ,  data = tab_pob_ref, nest = TRUE)
boot %>% summary()
boot_design <- as.svrepdesign(boot, type = "bootstrap", replicates = m.boot)
porc.survey <- svyratio(~I(P11_12_5 == 1), ~as.numeric(edad_num>=15), design = boot_design)
porc.survey
cv(porc.survey)
confint(porc.survey)[1]


tab.res <- tab.res %>% 
  bind_rows(tibble( tipo = "survey", 
                   prom = porc.survey$ratio[1],
                   median = NA,
                   q975 = confint(porc.survey)[2],
                   q025 = confint(porc.survey)[1]))
tab.res

ggplot(tab.res, aes(y = tipo, x = prom)) + 
  geom_errorbarh(aes(xmin = q025, xmax = q975), 
                height = .3) + 
  geom_point(color = "red", size = 2)


