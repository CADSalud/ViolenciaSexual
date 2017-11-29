

library(ProjectTemplate)
library(bigrquery)


# Conexión a prueba
project <- "acoustic-field-186719" # put your project ID here
sql <- "SELECT * FROM [acoustic-field-186719:nacimientos.nacim2011] LIMIT 10"
tt <- query_exec(sql, project = project)

dim(tt)
tt
