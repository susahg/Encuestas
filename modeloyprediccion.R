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
  modpp <- lm(dataset[dataset$partido == "pp",]$sesgo ~ dataset[dataset$partido == "pp",]$empresaymedio)
  mod_inter_pp <- as.data.frame(merge(confint(modpp), modpp$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_pp) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_pp <- mod_inter_pp[-1,]
  mod_inter_pp$empresaymedio <- lapply(mod_inter_pp$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_pp$partido <- "pp"
  
  
  modpsoe <- lm(dataset[dataset$partido == "psoe",]$sesgo ~ dataset[dataset$partido == "psoe",]$empresaymedio)
  mod_inter_psoe <- as.data.frame(merge(confint(modpsoe), modpsoe$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_psoe) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_psoe <- mod_inter_psoe[-1,]
  mod_inter_psoe$empresaymedio <- lapply(mod_inter_psoe$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_psoe$partido <- "psoe"
  
  modcs <- lm(dataset[dataset$partido == "cs",]$sesgo ~ dataset[dataset$partido == "cs",]$empresaymedio)
  mod_inter_cs <- as.data.frame(merge(confint(modcs), modcs$coefficients, by = "row.names", all = TRUE))
  colnames(mod_inter_cs) <- c("empresaymedio","lower","upper","sesgodelmedio")
  mod_inter_cs <- mod_inter_cs[-1,]
  mod_inter_cs$empresaymedio <- lapply(mod_inter_cs$empresaymedio,function(x) {gsub(".*ymedio", "", x)})
  mod_inter_cs$partido <- "cs"
  
  modpodemos <- lm(dataset[dataset$partido == "podemos",]$sesgo ~ dataset[dataset$partido == "podemos",]$empresaymedio)
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
  
  
  
  valor <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgodelmedio"])
  
  #print(paste0(valor,"valor"))
  #print(paste0(elparty,"partido"))
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

medios <- c("ABC (GAD3)","GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Última Hora (IBES)")
suerte <- suerte[!(suerte$empresaymedio %in% medios),]

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


####### SIN  LM


predi_intencion_voto_NOLM <- function(ultimo_dia,elparty,elmedio,dataset,ventana,real){
  
  encuestas_train <- dataset[dataset$fecha < ultimo_dia,]
  
  
  media_sesgo <- dataset %>%
    group_by(partido,empresaymedio) %>%
    summarise(sesgodelmedio = mean(sesgo*100),sesgo_2sd=2*sd(sesgo*100))
  
  
  
  
  consenso_partido <- sesgo_medio(ultimo_dia,ventana,elparty,encuestas)
  
  
  
  valor <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgodelmedio"])
  
  #print(paste0(valor,"valor"))
  #print(paste0(elparty,"partido"))
  #print(paste0(elmedio,"medio"))
  
  #valor_inf <- (consenso_partido$consenso*100) - as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  #valor_sup <- (consenso_partido$consenso*100) + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  
  valor_inf <- valor - as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  valor_sup <- valor + as.numeric(media_sesgo[media_sesgo$empresaymedio == elmedio & media_sesgo$partido == elparty,"sesgo_2sd"])
  
  
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



suerte <- encuestas %>%
  group_by(empresaymedio,partido) %>%
  filter(fecha == max(fecha)) %>%
  filter(fecha >= "2017-05-01" ) 

suerte <- suerte[,c("fecha","partido","empresaymedio","intencionvoto")]
suerte$intencionvoto <- suerte$intencionvoto*100
suerte <- as.data.frame(suerte)

medios <- c("ABC (GAD3)","GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Última Hora (IBES)")
suerte <- suerte[!(suerte$empresaymedio %in% medios),]

#(ultimo_dia,elparty,elmedio,dataset,ventana,real)
prediccion_final <- apply(suerte,1, function(x){
  #print (as.Date(x['fecha']))
  #print (x['partido'])
  #print (x['empresaymedio'])
  #print (as.numeric(x['intencionvoto']))
  predi_intencion_voto_NOLM(as.Date(x['fecha']),x['partido'],x['empresaymedio'],final,60,as.numeric(x['intencionvoto']))
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

