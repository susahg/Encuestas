
#datos ELMundo
source("funct_read_elmundo.R")
encu_elmundo <- funct_read_elmundo()

#datos Wikipedia
source("funct_read_wiki.R", encoding = "UTF-8")
encu_wiki <- funct_read_wiki()


encuestas <- rbind(encu_wiki,encu_elmundo)
encuestas <- encuestas[with(encuestas, order(partido, fecha)), ]
encuestas <- encuestas[!duplicated(encuestas[,c("empresaymedio","fecha","partido")]),]


# limpieza
tmp <- Filter(function(x) is.data.frame(get(x)), ls())
tmp <- tmp[tmp != "encuestas"]
rm(list=tmp)

