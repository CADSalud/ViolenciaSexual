
library(ProjectTemplate)
load.project()


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


data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

## domain means are ratio estimates, but available directly
svyratio(~I(api.stu*(comp.imp=="Yes")), ~as.numeric(comp.imp=="Yes"), dstrat)
svymean(~api.stu, subset(dstrat, comp.imp=="Yes"))

## separate and combined ratio estimates of total
(sep<-svyratio(~api.stu,~enroll, dstrat,separate=TRUE))
(com<-svyratio(~api.stu, ~enroll, dstrat))

stratum.totals<-list(E=1877350, H=1013824, M=920298)

predict(sep, total=stratum.totals)
predict(com, total=sum(unlist(stratum.totals)))

SE(com)
coef(com)
coef(com, drop=FALSE)
confint(com)

# 1. Población de referencia ----
load("cache/tab_union.RData")
tab_union %>% data.frame() %>% head
tab_union$P11_12_5 %>% summary()

tab_pob_ref <- tab_union %>% 
  dplyr::select(ID_MUJ, ID_VIV, 
                UPM, FAC_MUJ, 
                edad_num, edad_gpo, P11_12_5) %>% 
  mutate(resp_si = P11_12_5 == 1)
tab_pob_ref
tab_pob_ref %>% 
  group_by(P11_12_5) %>% 
  summarise(npond = sum(FAC_MUJ)) %>% 
  ungroup() %>% 
  mutate(porc = npond/sum(npond))

# 2. Bootstrap -----
boot <- svydesign(id = ~1, weights = ~FAC_MUJ,  data = tab_pob_ref, nest = TRUE)
boot %>% summary()
boot_design <- as.svrepdesign(boot, type = "bootstrap", replicates = 100)
tt <- svyratio(~I(P11_12_5 == 1), ~as.numeric(edad_num>=15), design = boot_design)
tt
confint(tt)
