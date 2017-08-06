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



#encuestas <- read.csv("encuestas.csv", stringsAsFactors = FALSE, header = T, fileEncoding = "latin1")
#encuestas <- encuestas[,-1]
#encuestas$fecha <- as.Date(encuestas$fecha)


#### n?mero de encuestas que tenemos ... haremos una selecci?n de las que m?s casos tenemos

encuestas %>%
  #select(empresaymedio) %>%
  #distinct() %>%
  group_by(empresaymedio) %>%
  summarise(Encuestas = n()) %>%
  ungroup() %>%
  mutate(empresaymedio = fct_reorder(empresaymedio, Encuestas)) %>%
  ggplot(aes(x = Encuestas, y = empresaymedio)) +
  geom_point() +
  theme(legend.position = "none")

table(encuestas %>%
        #select(empresaymedio) %>%
        #distinct() %>%
        group_by(empresaymedio) %>%
        summarise(Encuestas = n()) %>%
        ungroup() %>%
        mutate(empresaymedio = fct_reorder(empresaymedio, Encuestas)))


##############################################

#funciÃ³n calcula consenso
sesgo_medio <- function(hastaaqui, ventana,el_partido){
  
  # data for party 
  data_id <- encuestas %>%
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
    #  mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=4), 
    #             family = "quasibinomial", data = thedata)
    #  print ("hola391")}
      mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=4), 
                 family = "quasibinomial", data = thedata)
    #mod <- gam(intencionvoto ~ s(as.numeric(fecha), bs="cr"), 
    #             family = "quasibinomial", data = thedata)
    #mod <- gam(intencionvoto ~ t2(as.numeric(fecha),k=4, bs="cr"), 
    #             family = "quasibinomial", data = thedata)
    
     
    preddata2 <- data_id[data_id$id %in% hastaaquideverdad, c("fecha","empresaymedio")]

    preddata <- as.data.frame(data_id[data_id$id %in% hastaaquideverdad, "fecha"])
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
  sesgo_medio(as.Date(x['Var1']),60,x['Var2'])
})

df <- data.table(do.call("rbind", params))

final <- merge(encuestas,df, by.x=c("fecha","empresaymedio","partido"), by.y=c("fecha","medio","partido"))

final$sesgo <- final$intencionvoto - final$consenso

medios <- c("GESOP/El PeriÃ³dico","GIPEyOP/Mediaflows","JJD/lainformacion.com","La Vanguardia (GAD3)","Redondo&Asociados","20 Minutos (A+M)","Libertad Digital (Demoscopia y Servicios","Llorente & Cuenca (IMOP)","NC Report","Resultado elecciones","Ãltima Hora (IBES)")
final <- final[!(final$empresaymedio %in% medios),]





###### dibujamos consenso que es nuestro predcit del m?todo GAM
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

################ Gráfico sesgo


final%>%ggplot(aes(x=partido,y=sesgo*100,fill=empresaymedio))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~partido,scales="free_x")+
  #theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Partido")+ylab("Sesgo")+scale_fill_discrete(name="Pollster")+
  theme_bw()+theme(legend.position="bottom")


final%>%filter(partido%in%c("pp","psoe","podemos","cs")&!is.na(sesgo))%>%
  ggplot(aes(x=fecha,y=sesgo*100,colour=partido))+geom_point()+geom_line()+facet_wrap(~empresaymedio)+
  theme(axis.text.x=element_text(angle=90,hjust=1))



final%>%filter(partido%in%c("pp","psoe","podemos","cs")&!is.na(sesgo))%>%
  ggplot(aes(x=empresaymedio,y=sesgo*100,fill=empresaymedio))+geom_boxplot()+facet_wrap(~partido)+
  theme(axis.text.x=element_text(angle=90,hjust=1))


final%>%filter(partido%in%c("pp","psoe","podemos","cs")&!is.na(sesgo))%>%
  ggplot(aes(x=partido,y=sesgo*100,fill=partido))+geom_boxplot()+facet_wrap(~empresaymedio)+
  theme(axis.text.x=element_text(angle=90,hjust=1))
dev.off()







######### calculamos un lm del sesgo por el medio para obtener los intervalos de confianza de esos valores 

modpp <- lm(final[final$partido == "pp",]$sesgo ~ final[final$partido == "pp",]$empresaymedio)
#confint(modpp)

modpsoe <- lm(final[final$partido == "psoe",]$sesgo ~ final[final$partido == "psoe",]$empresaymedio)
#confint(modpsoe)


modcs <- lm(final[final$partido == "cs",]$sesgo ~ final[final$partido == "cs",]$empresaymedio)
#confint(modcs)

modpodemos <- lm(final[final$partido == "podemos",]$sesgo ~ final[final$partido == "podemos",]$empresaymedio)
#confint(modpodemos)




########## gráficos previos incluir en el rmd 

house_colours <- c("orange","purple","blue","red")

names(house_colours) <-   c("cs", "podemos", "pp", "psoe")


#pdf("figures/ge2015_votingIntention.pdf", 6, 4)
ggplot(encuestas, aes(x=fecha, y=intencionvoto*100, col=partido)) +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  geom_smooth(aes(fill=partido), col=I("grey65"), show_guide=F,method = "gam") +
  scale_color_manual(values=house_colours) +
  scale_fill_manual(values=house_colours) +
  labs(col="", y="Intenci?n voto (Loess)", x="") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-12-20"))),
             linetype=5, colour="red") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-06-26"))),
             linetype=5, colour="red") +
  #geom_vline(aes(xintercept=as.numeric(as.POSIXct("2016-06-25")),linetype=4, colour="black")) +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="horizontal", keywidth=.8)) +
  theme_bw()+theme(legend.position="bottom")
dev.off()  


# commisioned by... n.7
newspapers <- c("La Raz?n (NC Report)", "El Pa?s (Metroscopia)", "ABC (GAD3)", "El Mundo (Sigma Dos)")


encuestas$newspaper <- NA

for(n in newspapers){
  encuestas$newspaper[which(n == encuestas$empresaymedio)]  <- n
}

encuestas$newspaper <- as.factor(encuestas$newspaper)
counts <- encuestas %>% group_by(newspaper) %>% tally()
pick <- counts[which(counts$n > 50),]$newspaper

ggplot(subset(encuestas, newspaper %in% pick),
       aes(x=fecha, y=intencionvoto*100, col=newspaper, group=newspaper)) +
  geom_jitter(alpha=I(.5), size=I(1.4)) +
  facet_wrap(~partido, scales="free_y") +
  geom_smooth(method="loess", se=F, show_guide=F, size=1.2) +
  geom_vline(aes(xintercept=as.numeric(as.Date("2015-12-20"))),
             linetype=5, colour="red") +
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-06-26"))),
             linetype=5, colour="red") +
  scale_color_brewer(type="qual", palette="Set2") +
  scale_fill_brewer(type="qual", palette="Set2") +
  labs(col="Newspaper", y="Voting intention (%)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="vertical", keywidth=.8, ncol=1)) +
  theme(legend.position=c(.95, .90))

############# para doc n.8
encuestas$intencionvoto <- encuestas$intencionvoto*100
colnames(encuestas) <- c("inst","muestra","date","margendeerror","party","value")

X <- subset(encuestas, encuestas$party == 'pp')
mediana <- get_median(X)

mediana$date <- as.Date(mediana$date)
X <- subset(X, encuestas$inst == 'La Raz?n (NC Report)' | encuestas$inst == 'El Pa?s (Metroscopia)')

X%>%filter(!is.na(inst))%>%ggplot(aes(x=date, y=value))+
  geom_jitter(alpha=I(.3), size=I(1.4),col="blue") +
  geom_line(aes(x=date,y=value),col="red",data=mediana) +
  facet_wrap(~inst,scales="free_x")+
  #theme(axis.text.x=element_text(angle=90,hjust=1))+
  #xlab("Partido")+ylab("Sesgo")+scale_fill_discrete(name="Pollster")+
  theme(axis.text.x=element_text(angle=90,hjust=1))




