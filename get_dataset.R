library(jsonlite)
library(tidyr)
library(reshape2)
library(data.table)

setwd("~/TFM/SRC")
datajson <- fromJSON("https://spreadsheets.google.com/feeds/list/1ZLjZikluov2o5ZJkpgGzDO5bTg34eDKyM1H2OQ445BM/od6/public/basic?alt=json")

#separa por ", " "pp:3,1, psoe:2.2,...."
items <- strsplit(datajson$feed$entry$content$`$t`,", ")
#separamos el nombre de las variables de su valor
items <- lapply(items, strsplit, ": ")
#hacemos pequeñas matrices en una columna los nombres y en la otra los valores 
lista <- unlist(items, recursive = FALSE)
#generamos un data frame con 2 col, uno los nombres y otros los valores
df <- data.table(do.call("rbind", lista))
#creamos la vble time para poder generar el data frame con 9 cols y 265 obs
df$time <- rep(1:length(items),as.vector(sapply(items,length)))
df <- dcast(df, time ~ V1, value.var = "V2")

df$fecha <- as.character(datajson$feed$entry$title$`$t`)
df$time <- NULL

cols <- c("margendeerror","pp","psoe","cs","podemos","iu")
df <- as.data.frame(df)
df[,cols] <- lapply(df[,cols],function(x) {as.numeric(gsub(",",".",x))})
df[,"fecha"] <- as.Date(df[,"fecha"],format= "%d/%m/%Y")

### problema con 2 valores q tienen un punto
df[,"tamañomuestra"] <- sapply(df[,"tamañomuestra"],function(x) {gsub(".","",x,fixed=T)})
df$muestra<- as.numeric(df$tamañomuestra)
df$tamañomuestra <- df$iu<- NULL
df <- df[order(df$fecha),]

### para es_encuestas(NZ)
encuestas <-gather(partido,intencionvoto, -empresaymedio, -muestra,-margendeerror,-fecha,data=df)
encuestas$intencionvoto <- encuestas$intencionvoto/100
encuestas$empresaymedio[encuestas$muestra>=20000000] <- "Resultado elecciones"
rm(df)

sapply(df,class)
summary(df)
str(df)

#### tamaño muestra

table(df$muestra)


#####################

#library(jsonlite)
#library(plyr)

#datajson <- fromJSON("https://spreadsheets.google.com/feeds/list/1ZLjZikluov2o5ZJkpgGzDO5bTg34eDKyM1H2OQ445BM/od6/public/basic?alt=json")

#tmp <- datajson$feed[[12]]
#tmp <- tmp$content[,2]

#tmp <- strsplit(tmp, ", ")
#tmp <- lapply(tmp, strsplit, ": ")
#tmp <- lapply(tmp, function(x) do.call(rbind, x))

#res <- ldply(tmp, function(x) {
#  res <- as.list(x[,2])
#  names(res) <- x[,1]
#  as.data.frame(res)
#})


################################# wikipediA
library("xml2")
library("rvest")

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_Spanish_general_election,_2016"

webpage <- url %>%
  read_html(encoding = "UTF-8") 

tabs <- webpage %>%
  html_nodes("table") 

tab1 <- html_table(tabs[[1]], fill = TRUE) 
tab2 <- html_table(tabs[[2]], fill = TRUE) 

tab2 <- 

tab <- rbind(tab1,tab2)
View(tab)

tab <- tab[tab[ , 1] != tab[ , 2], ]
tab <- tab[tab[ , 1] != "Poll", ]
names(tab)[2] <- "WikipediaDates"

tab$WikipediaDates



