library(ProjectTemplate)
load.project()

library(stringi)

### 1. Carga los datos de egresos (Secretaría de Salud y Servicios Estatales de Salud)----
egreso <- read_csv('data/SAEH/EGRESOS/Base de Datos/EGRESO.csv') #2,959,197

cache('egreso')  

### 2. Carga catálogo CIE-10 ----
catcie10 <- read.csv('data/SAEH/EGRESOS/Cat†logos/CATCIE10.csv', 
                     sep = '|', 
                     stringsAsFactors = FALSE) %>% 
  mutate(cve_diag =  stri_pad_right(gsub("[^0-9A-Za-z///' ]", "", CAUSA),
                                    width = 4,
                                    pad = "X"), 
         #stri_enc_detect("Fractura del calc\xe1neo")
         diagnostico = stri_encode(Nombre, "ISO-8859-1", "UTF-8"))

cache('catcie10')

### 3. Tabla de egresos hospitalarios de adolescentes con sus causas (SS)----
egreso_adoles <- egreso %>% 
  mutate(redad = ifelse(EDAD > 9 & EDAD < 15,'adoles_temp',
                        ifelse(EDAD > 14 & EDAD < 20, 'adoles_tard', -1)),
         genero = ifelse(SEXO == 1, 'masculino',
                         ifelse(SEXO == 2, 'femenino', -1)),
         motivo_egre = case_when(.$MOTEGRE == 1 ~ 'curación',
                                 .$MOTEGRE == 2 ~ 'mejoría',
                                 .$MOTEGRE == 3 ~ 'voluntario',
                                 .$MOTEGRE == 4 ~ 'pase a otro hospital',
                                 .$MOTEGRE == 5 ~ 'defunción',
                                 .$MOTEGRE == 6 ~ 'otro motivo',
                                 .$MOTEGRE == 9 ~ 'no especificado'),
         intencion = case_when(.$TRAUMAT == 1 ~ 'accidental',
                               .$TRAUMAT == 2 ~ 'violencia familiar',
                               .$TRAUMAT == 3 ~ 'violencia no familiar',
                               .$TRAUMAT == 4 ~ 'autoinflingido',
                               .$TRAUMAT == 9 ~ 'se ignora')) %>% 
  filter(redad != -1, genero != -1) %>% 
  select(id = ID,
         clues = CLUES,
         dias_esta = DIAS_ESTA,
         edad = EDAD,
         redad,
         genero,
         ent = ENTIDAD,
         mun = MUNIC,
         motivo_egre,
         intencion,
         mes = MES_ESTADISTICO,
         cve_diag_ini = DIAG_INI,
         cve_afec_prin = AFECPRIN,
         cve_causa_ext = CAUSAEXT)

cache('egreso_adoles')
  
### 4. Carga los datos de sectorial (IMSS, ISSSTE, etc) ----
sectorial <- read_csv('data/SAEH/BDSS_2014/SECTORIAL2014.csv') #6,368,082

cache('sectorial')

### 5. Tabla de egresos hospitalarios de adolescentes con sus causas (sectorial) ----
sectorial_adoles <- sectorial %>% 
  mutate(redad = ifelse(EDAD1 > 9 & EDAD1 < 15,'adoles_temp',
                        ifelse(EDAD1 > 14 & EDAD1 < 20, 'adoles_tard', -1)),
         genero = ifelse(SEXO == 1, 'masculino',
                         ifelse(SEXO == 2, 'femenino', -1)),
         motivo_egre = case_when(.$MOTEGRE == 1 ~ 'curación',
                                 .$MOTEGRE == 2 ~ 'mejoría',
                                 .$MOTEGRE == 3 ~ 'voluntario',
                                 .$MOTEGRE == 4 ~ 'pase a otro hospital',
                                 .$MOTEGRE == 5 ~ 'defunción',
                                 .$MOTEGRE == 6 ~ 'otro motivo',
                                 .$MOTEGRE == 9 ~ 'no especificado'),
         institucion = case_when(.$DH == 1 ~ 'Secretaría de Salud',
                                 .$DH == 2 ~ 'IMSS',
                                 .$DH == 3 ~ 'IMSS Oportunidades',
                                 .$DH == 4 ~ 'ISSSTE',
                                 .$DH == 5 ~ 'PEMEX',
                                 .$DH == 6 ~ 'SEMAR')) %>% 
  filter(redad != -1, genero != -1) %>% 
  select(id = ID,
         institucion,
         clues = CLUES,
         dias_esta = DIAS_ESTA,
         edad = EDAD1,
         redad,
         genero,
         ent_atn = CEDOCVE,
         motivo_egre,
         cve_afec_prin = AFECPRIN4,
         cve_causa_bas = CAUSABAS4)
  
cache('sectorial_adoles')  
