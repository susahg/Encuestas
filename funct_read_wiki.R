################################# wikipediA
library("xml2")
library("rvest")
library("tidyr")

funct_read_wiki <- function() {




url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election"

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

tab <- html_table(tabs[[1]], fill = TRUE) 



tab <- tab[tab[ , 1] != tab[ , 2], ]
tab <- tab[tab[ , 2] != "Fieldwork date", ]
#names(tab)[2] <- "WikipediaDates"


colnames(tab) <- c("Polling firm/Commissioner", "WikipediaDates", "muestra", "Turnout", "pp",
                   "psoe", "podemos", "cs","" ,"","","","","","","margendeerror")


tab$prueba <- strtrim(tab$WikipediaDates,20)


tab$fecha <- unlist(lapply(tab$prueba,function(x) {gsub(".*[–]", "", x)}))
tab[,"fecha"] <- as.character(gsub("\\s","",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Jan","01",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Feb","02",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Mar","03",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Apr","04",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("May","05",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Jun","06",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Jul","07",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Aug","08",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Sep","09",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Oct","10",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Nov","11",tab[,"fecha"]))
tab[,"fecha"] <- as.character(gsub("Dec","12",tab[,"fecha"]))
tab$fecha <- unlist(lapply(tab[,"fecha"],function(x) {ifelse(nchar(x)<8,paste0("0",x),x)}))
tab[,"fecha"] <- as.Date(tab[,"fecha"],format= "%d%m%Y")

tab$empresaymedio <- unlist(lapply(tab$`Polling firm/Commissioner`,function(x) {gsub("[[].*","",x)}))

cols <- c("pp","psoe","cs","podemos")
tab[,cols] <- lapply(tab[,cols],function(x) {as.numeric(substr(x,1,4))})

tab$muestra <- unlist(lapply(tab$muestra,function(x) {gsub(",","",x)}))

cols <- c("muestra","margendeerror")
tab[,cols] <- lapply(tab[,cols],function(x) {as.numeric(x)})


tab$empresaymedio[tab$empresaymedio == "Invymark/laSexta"] <- "La Sexta (Invymark)"
tab$empresaymedio[tab$empresaymedio == "MyWord/Cadena SER"] <- "SER (MyWord)"
tab$empresaymedio[tab$empresaymedio == "NC Report/La RazÃ³n"] <- "La RazÃ³n (NC Report)"
tab$empresaymedio[tab$empresaymedio == "DYM/El Confidencial"] <- "El Confidencial (DYM)"
tab$empresaymedio[tab$empresaymedio == "Celeste-Tel/eldiario.es"] <- "eldiario.es (Celeste-Tel)"
tab$empresaymedio[tab$empresaymedio == "Metroscopia/El PaÃ?s"] <- "El PaÃ?s (Metroscopia)"
tab$empresaymedio[tab$empresaymedio == "Sigma Dos/El Mundo"] <- "El Mundo (Sigma Dos)"
tab$empresaymedio[tab$empresaymedio == "GAD3/ABC"] <- "ABC (GAD3)"

cols <- c("empresaymedio","muestra","pp","psoe","cs","podemos","fecha","margendeerror")
tab <- tab[-nrow(tab),cols]
tab <- tab[order(tab$fecha),]
dat_wiki <-gather(partido,intencionvoto, -empresaymedio, -muestra,-margendeerror,-fecha,data=tab)
dat_wiki$intencionvoto <- dat_wiki$intencionvoto/100

return(dat_wiki)

}

