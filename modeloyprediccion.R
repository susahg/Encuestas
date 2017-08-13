library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(xts)
library(data.table)

##############################################
######## MODELO

#función calcula consenso
sesgo_medio <- function(hastaaqui, ventana,el_partido,dataset){
  
  # data for party 
  data_id <- dataset %>%
    filter(partido == el_partido)
  data_id$id <- row.names(data_id)
  
  # si encontramos la fecha nos quedamos hasta el valor  ...  ESTO PARA PREDICCIÓN IMPLEMENTAR
  #if (sum(data_id$fecha != as.Date(hastaaqui)) > 0) {
  # x <- as.Date(hastaaqui)
  # q <- which(abs((data_id$fecha-x)) == min(abs((data_id$fecha - x))) & (data_id$fecha - x) <= 0)
  # hastaaquideverdad <- data_id[data_id$id %in% max(q), "fecha"]
  #} else{
  
  # si la fecha-1 es repe nos quedamos con el útlimo id para que entren en el dataframe
  idhastaaqui <- as.numeric(min(data_id[data_id$fecha %in% as.Date(hastaaqui), "id"])) - 1
  x <- data_id[data_id$id %in% idhastaaqui, "fecha"]
  q <- which(abs((data_id$fecha-x)) == min(abs((data_id$fecha - x))) & (data_id$fecha - x) <= 0)
  hastaaquideverdad <- as.numeric(data_id[data_id$id %in% max(q), "id"])
  #}
  
  
  
  # abrimos ventana de 60 y calculamoes el modelo sin ese punto
  thedata <- data_id[data_id$id %in% seq(hastaaquideverdad-ventana,hastaaquideverdad), ]
  
  #mod <- gam(intencionvoto ~ s(as.numeric(fecha)) + empresaymedio, 
  #             family = "quasibinomial", data = thedata)
  
  #mod <- gam(intencionvoto ~ s(as.numeric(fecha)), 
  #             family = "quasibinomial", data = thedata)
  #mod <- gam(intencionvoto ~ t2(as.numeric(fecha)), 
  #           family = "quasibinomial", data = thedata)
  #if (i == nrow(data_id) & data_id[data_id$id == i,"dupli"] == F)  {
  mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=3), 
               family = "quasibinomial", data = thedata)
  #  print ("hola391")}
  #mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=4), 
  #           family = "quasibinomial", data = thedata)
  #mod <- gam(intencionvoto ~ s(as.numeric(fecha), bs="cr"), 
  #             family = "quasibinomial", data = thedata)
  #mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=4, bs="cr"), 
  #             family = "quasibinomial", data = thedata)
  
  
  preddata2 <- data_id[data_id$id %in% (idhastaaqui+1), c("fecha","empresaymedio")]
  
  preddata <- as.data.frame(data_id[data_id$id %in% (idhastaaqui+1), "fecha"])
  colnames(preddata) <- "fecha"
  
  
  #con el modelo damos la predicción de ese punto (consenso)
  sesgomedio <- predict(mod, newdata = preddata, type = "response") 
  
  res <- data.frame(consenso = sesgomedio,
                    partido = el_partido,
                    #fecha = as.Date(preddata$fecha,format= "%Y-%m-%d"),
                    fecha = as.Date(preddata2$fecha,format= "%Y-%m-%d"),
                    medio = preddata2$empresaymedio)
  res
  
}



elpartido <- c("pp","psoe","cs","podemos")
ventana <- 60 
fechas <- encuestas[(ventana+1):((nrow(encuestas)/4)), "fecha"]
prueba <- expand.grid(fechas,elpartido)

#### calcula el sesgo para cada id del data frame posterior a la ventana, por partido.

params <- apply(prueba,1, function(x){
  sesgo_medio(as.Date(x['Var1']),60,x['Var2'],encuestas)
})

df <- data.table(do.call("rbind", params))

final <- merge(encuestas,df, by.x=c("fecha","empresaymedio","partido"), by.y=c("fecha","medio","partido"))

final$sesgo <- final$intencionvoto - final$consenso

medios <- c("GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Última Hora (IBES)")
final <- final[!(final$empresaymedio %in% medios),]





###### dibujamos consenso que es nuestro predcit del método GAM
house_colours <- c("orange","purple","blue","red")

names(house_colours) <-   c("cs", "podemos", "pp", "psoe")


ggplot(final, aes(x=fecha, y=intencionvoto*100, col=partido)) +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  geom_line(aes(y=consenso*100),data=final) +
  scale_color_manual(values=house_colours) +
  #scale_fill_manual(intencionvoto=palette) +
  labs(col="", y="Intenci?n voto predicha (GAM)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="horizontal", keywidth=.8)) +
  theme_bw()+theme(legend.position="bottom")



### para predecir
predi_intencion_voto <- function(ultimo_dia,elparty,elmedio,dataset,ventana,real){
  
  encuestas_train <- dataset[dataset$fecha < ultimo_dia,]
 
  
  #calculamos los intervalos de confianza y los sesgo para sumar al consenso
  #hasta la fecha anterior al útlimo día que tenemos de encuestas
  modpp <- lm(encuestas_train[encuestas_train$partido == "pp",]$sesgo ~ encuestas_train[encuestas_train$partido == "pp",]$empresaymedio -1)
  mod_inter_pp <- as.data.frame(merge(confint(modpp), modpp$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_pp) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_pp <- mod_inter_pp[-1,]
  mod_inter_pp$empresaymedio <- lapply(mod_inter_pp$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_pp$partido <- "pp"
  
  
  modpsoe <- lm(encuestas_train[encuestas_train$partido == "psoe",]$sesgo ~ encuestas_train[encuestas_train$partido == "psoe",]$empresaymedio-1)
  mod_inter_psoe <- as.data.frame(merge(confint(modpsoe), modpsoe$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_psoe) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_psoe <- mod_inter_psoe[-1,]
  mod_inter_psoe$empresaymedio <- lapply(mod_inter_psoe$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_psoe$partido <- "psoe"
  
  modcs <- lm(encuestas_train[encuestas_train$partido == "cs",]$sesgo ~ encuestas_train[encuestas_train$partido == "cs",]$empresaymedio-1)
  mod_inter_cs <- as.data.frame(merge(confint(modcs), modcs$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_cs) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_cs <- mod_inter_cs[-1,]
  mod_inter_cs$empresaymedio <- lapply(mod_inter_cs$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_cs$partido <- "cs"
  
  modpodemos <- lm(encuestas_train[encuestas_train$partido == "podemos",]$sesgo ~ encuestas_train[encuestas_train$partido == "podemos",]$empresaymedio-1)
  mod_inter_podemos <- as.data.frame(merge(confint(modpodemos), modpodemos$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_podemos) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_podemos <- mod_inter_podemos[-1,]
  mod_inter_podemos$empresaymedio <- lapply(mod_inter_podemos$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_podemos$partido <- "podemos"
  
  ### unimos en un solo dataset
  media_sesgo <- Reduce(function(x, y) rbind(x, y), 
                        list(mod_inter_podemos, mod_inter_pp, mod_inter_psoe, mod_inter_cs))
  cols <- c("lower","upper","sesgodelmedio")
  media_sesgo[,cols] <- lapply(media_sesgo[,cols],function(x) {as.numeric(x*100)})
  
  
  consenso_partido <- sesgo_medio(ultimo_dia,ventana,elparty,encuestas)
  print (media_sesgo)
  #print (consenso_partido$consenso*100)
  
  valor <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgodelmedio"])
  
  print(paste0(valor,"valor"))
  print(as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgodelmedio"]))
  #print(paste0(elmedio,"medio"))
  
  valor_inf <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"lower"])
  valor_sup <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"upper"])
  
  #print(paste0(valor_inf,"valor_inf"))
  #print(paste0(valor_sup,"valor_sup"))
  
  muchasuerte <- data.frame(partido = elparty,
                            medio = elmedio,
                            fecha = as.Date(ultimo_dia,format= "%Y-%m-%d"),
                            intencionvoto = real,
                            predicho=valor,
                            lower=valor_inf,
                            upper=valor_sup)
  
  muchasuerte
  
  
}



### dataset resultado
suerte <- encuestas %>%
  group_by(empresaymedio,partido) %>%
  filter(fecha == max(fecha)) %>%
  filter(fecha >= "2017-05-01" ) 

suerte <- suerte[,c("fecha","partido","empresaymedio","intencionvoto")]
suerte$intencionvoto <- suerte$intencionvoto*100
suerte <- as.data.frame(suerte)

#medios <- c("ABC (GAD3)","GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Última Hora (IBES)")
#suerte <- suerte[!(suerte$empresaymedio %in% medios),]
#medios <- c("CIS","SER (MyWord)","ABC (GAD3)","Antena 3 (TNS Demoscopia)","Simple Lógica","El País (Metroscopia)","La Razón (NC Report)","eldiario.es (Celeste-Tel)","La Sexta (Invymark)")
medios <- c("CIS","SER (MyWord)","Antena 3 (TNS Demoscopia)","Simple Lógica","El País (Metroscopia)","La Razón (NC Report)","eldiario.es (Celeste-Tel)","La Sexta (Invymark)")
suerte <- suerte[(suerte$empresaymedio %in% medios),]


#(ultimo_dia,elparty,elmedio,dataset,ventana,real)
prediccion_final <- apply(suerte,1, function(x){
  #print (as.Date(x['fecha']))
  #print (x['partido'])
  #print (x['empresaymedio'])
  #print (as.numeric(x['intencionvoto']))
  predi_intencion_voto(as.Date(x['fecha']),x['partido'],x['empresaymedio'],final,60,as.numeric(x['intencionvoto']))
})


mas_suerte <- data.table(do.call("rbind", prediccion_final))

mas_suerte$acierto <- ifelse((mas_suerte$intencionvoto >= mas_suerte$lower & mas_suerte$intencionvoto <= mas_suerte$upper),"BIEN","MAL")
table(mas_suerte$acierto)



d <- mas_suerte %>%
  mutate(org = fct_reorder(medio,upper),
         ypos = as.numeric(org))



d %>%
  ggplot(aes(x=lower,y = ypos, colour = medio)) +
  #geom_segment(aes(yend = ypos, x = lower, xend = upper)) +
  geom_segment(aes(xstart=lower ,xend=upper,ystart=ypos,yend=ypos),size=2) +
  geom_point(aes(x = predicho, y = ypos), size = 2,col="red") +
  geom_point(aes(x = intencionvoto, y = ypos), size = 2,col="black") +
  facet_wrap(~partido, scales="free_y") +
  #scale_y_continuous(breaks = 1:5, labels = levels(d$org),
  #                   minor_breaks = NULL) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right") +
  labs(x = "",
       y = "",
       colour = "",
       caption = "")  +
  ggtitle("fatal",
          "")

house_colours <- c("orange","purple","blue","red")
names(house_colours) <-   c("cs", "podemos", "pp", "psoe")

ggplot(mas_suerte, aes(x = fecha, y = intencionvoto,col=partido)) +
  #geom_line(col = "blue") +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  facet_wrap(~partido, scales="free_y") +
  scale_color_manual(values=house_colours) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(aes(y = predicho,x = fecha), col = "black", alpha = 0.6) +
  #geom_line(aes(y = q90), col = "gray", alpha = 0.6) +
  theme_bw() 


### prueba
#library(dygraphs)
#
#
#for (i in c("cs","pp","psoe","podemos")) {
#
#mas_suerte[mas_suerte$partido=="pp",]
#
#cols <- c("lower","predicho","upper")
#plow <- xts(mas_suerte$lower, as.Date(mas_suerte$fecha, format='%Y-%m-%d'))
#pupper <- xts(mas_suerte$upper, as.Date(mas_suerte$fecha, format='%Y-%m-%d'))
#pfit <- xts(mas_suerte$predicho, as.Date(mas_suerte$fecha, format='%Y-%m-%d'))
#
#p <- cbind(plow,pfit,pupper)
#colnames(p) <- c("lwr", "fit", "upr")
#dygraph(p, main = "Predicted Lung Deaths (UK)") %>%
#  dySeries(c("lwr", "fit", "upr"), label = "Deaths")  %>% 
#  dyRangeSelector()

#}




####### SIN  LM


predi_intencion_voto_NOLM <- function(ultimo_dia,elparty,elmedio,dataset,ventana,real){
  
  #con ventana
  #data_id <- dataset %>%
  #  filter(partido == elparty)
  #data_id$id <- row.names(data_id)
  
  #print (data_id)
  
  #idultimodia <- (as.numeric(min(data_id[data_id$fecha %in% as.Date(ultimo_dia), "id"]))) - 1
  #print (idultimodia)
  #idultimod_menosventana <- as.numeric(min(data_id[data_id$fecha %in% as.Date(ultimo_dia), "id"])) - ventana
  #print (idultimod_menosventana)
  #encuestas_train <- data_id[data_id$id %in% seq(idultimod_menosventana,idultimodia), ]
  #print (encuestas_train)
  
  #sin ventana
  encuestas_train <- dataset[dataset$fecha < ultimo_dia,]
 
  print (elmedio)
  
  
  media_sesgo <- encuestas_train %>%
    #group_by(partido,empresaymedio) %>%
    filter(partido == elparty) %>%
    filter(empresaymedio == elmedio) %>%
    summarise(sesgodelmedio = mean(sesgo*100),sesgo_2sd=2*sd(sesgo*100))

  
  consenso_partido <- sesgo_medio(ultimo_dia,ventana,elparty,encuestas)
  #print (consenso_partido)
  
  is.na(media_sesgo$sesgo_2sd) <- 0
  
  #valor <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgodelmedio"])
  valor <- (consenso_partido$consenso*100) + as.numeric(media_sesgo$sesgodelmedio)
  
  #print(paste0(valor,"valor"))
  #print(paste0(elparty,"partido"))
  #print(paste0(elmedio,"medio"))
  
  valor_2sd <- as.numeric(media_sesgo$sesgo_2sd)
  
  #valor_inf <- (consenso_partido$consenso*100) - as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  #valor_sup <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  
  #valor_inf <- valor - as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  #valor_sup <- valor + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  
  
  #print(paste0(valor_inf,"valor_inf"))
  #print(paste0(valor_sup,"valor_sup"))
  
  muchasuerte <- data.frame(partido = elparty,
                            medio = elmedio,
                            fecha = as.Date(ultimo_dia,format= "%Y-%m-%d"),
                            intencionvoto = real,
                            predicho=valor,
                            #lower=valor_inf,
                            #upper=valor_sup
                            sd2 = valor_2sd)
  
  muchasuerte
  
  
}



suerte <- encuestas %>%
  group_by(empresaymedio,partido) %>%
  filter(fecha == max(fecha)) %>%
  filter(fecha >= "2017-05-01" ) 

suerte <- suerte[,c("fecha","partido","empresaymedio","intencionvoto")]
suerte$intencionvoto <- suerte$intencionvoto*100
suerte <- as.data.frame(suerte)

#medios <- c("ABC (GAD3)","GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Última Hora (IBES)")
medios <- c("CIS","SER (MyWord)","ABC (GAD3)","Antena 3 (TNS Demoscopia)","Simple Lógica","El País (Metroscopia)","La Razón (NC Report)","eldiario.es (Celeste-Tel)","La Sexta (Invymark)")
#suerte <- suerte[!(suerte$empresaymedio %in% medios),]
suerte <- suerte[(suerte$empresaymedio %in% medios),]

#(ultimo_dia,elparty,elmedio,dataset,ventana,real)
prediccion_final <- apply(suerte,1, function(x){
  #print (as.Date(x['fecha']))
  #print (x['partido'])
  #print (x['empresaymedio'])
  #print (as.numeric(x['intencionvoto']))
  predi_intencion_voto_NOLM(as.Date(x['fecha']),x['partido'],x['empresaymedio'],final,60,as.numeric(x['intencionvoto']))
})


#para pruebas
#elparty <- "pp"
#elmedio <- "ABC (GAD3)"
#ultimo_dia <- "2017-06-28"
#ventana <- 60
#dataset <- final
#real <- 30.7
#prueba <- predi_intencion_voto_NOLM(ultimo_dia,elparty,elmedio,dataset,ventana,real)


mas_suerte <- data.table(do.call("rbind", prediccion_final))

mas_suerte$lower <- mas_suerte$predicho - mas_suerte$sd2
mas_suerte$upper <- mas_suerte$predicho + mas_suerte$sd2


mas_suerte$acierto <- ifelse((mas_suerte$intencionvoto >= mas_suerte$lower & mas_suerte$intencionvoto <= mas_suerte$upper),"BIEN","MAL")
table(mas_suerte$acierto)

d <- mas_suerte %>%
  mutate(org = fct_reorder(medio,upper),
         ypos = as.numeric(org))



d %>%
  ggplot(aes(x=lower,y = ypos, colour = medio)) +
  #geom_segment(aes(yend = ypos, x = lower, xend = upper)) +
  geom_segment(aes(xstart=lower ,xend=upper,ystart=ypos,yend=ypos),size=2) +
  geom_point(aes(x = predicho, y = ypos), size = 2,col="red") +
  geom_point(aes(x = intencionvoto, y = ypos), size = 2,col="black") +
  facet_wrap(~partido, scales="free_y") +
  #scale_y_continuous(breaks = 1:5, labels = levels(d$org),
  #                   minor_breaks = NULL) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right") +
  labs(x = "",
       y = "",
       colour = "",
       caption = "")  +
  ggtitle("fatal",
          "")




ggplot(mas_suerte, aes(x = fecha, y = intencionvoto,col=partido)) +
  #geom_line(col = "blue") +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  facet_wrap(~partido, scales="free_y") +
  scale_color_manual(values=house_colours) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.3) +
  geom_line(aes(y = predicho,x = fecha), col = "black", alpha = 0.6) +
  #geom_line(aes(y = q90), col = "gray", alpha = 0.6) +
  theme_bw() 

