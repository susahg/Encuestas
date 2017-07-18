library(mgcv)
library(tidyverse)
library(scales)
library(magrittr)
library(forcats)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(boot)

## datos "El Mundo"
source("get_dataset.R")


attach(encuestas)


##############################################

#funci√≥n calcula consenso

sesgo_medio <- function(hastaaqui, ventana,partido){
  
 
  
  #for(j in 1:length(partidos)){
    el_partido = partido
    #por partido

    
    
    data_id <- encuestas %>%
      filter(partido == el_partido)
    data_id$id <- row.names(data_id)
    
    
    thedata <- data_id[data_id$id %in% seq(hastaaqui-ventana,hastaaqui-1), ]
    
    mod <- gam(intencionvoto ~ s(as.numeric(fecha)) + empresaymedio, 
                 family = "quasibinomial", data = thedata)
      
      
      # for predicting values, we only take the pollsters we have an interest in:
    preddata <- data_id[data_id$id %in% c(hastaaqui), c("fecha","empresaymedio")]
      
      
      # house effect is shown by the amount the predicted value from polling
      # is *more* than the actual vote.  So a positive score means the poll
      # overestimated the actual vote:
      #sesgomedio <- predict(mod, newdata = preddata, type = "response") 
      sesgomedio <- predict(mod, newdata = preddata, type = "link")
      res <- data.frame(consenso = sesgomedio,
                        partido,
                        fecha = as.Date(preddata$fecha,format= "%Y-%m-%d"),
                        medio = preddata$empresaymedio)
      res
      #return(data.frame(c(sesgomedio,partido,as.Date(preddata$fecha,format= "%Y-%m-%d"),preddata$empresaymedio)))
    #}
    
  }   
  
  
 


elpartido <- c("pp","psoe","cs","podemos")
ventana <- 60 
ids <- seq(ventana+1,(nrow(encuestas)/4)-1)

prueba <- expand.grid(ids,elpartido)

#### calcula el sesgo para cada id del data frame posterior a la ventana, por partido.
#### quito estos porque no los lee bien, tengo que arreglar los textos para que no den error
params <- apply(prueba,1, function(x){
  if (as.numeric(x['Var1']) != 72 & as.numeric(x['Var1']) != 89 & as.numeric(x['Var1']) != 94 & as.numeric(x['Var1']) != 111 & as.numeric(x['Var1']) != 114 & as.numeric(x['Var1']) != 119
                          & as.numeric(x['Var1']) != 120 & as.numeric(x['Var1']) != 136 & as.numeric(x['Var1']) != 137 & as.numeric(x['Var1']) != 165 & as.numeric(x['Var1']) != 174
                          & as.numeric(x['Var1']) != 201 & as.numeric(x['Var1']) != 214 & as.numeric(x['Var1']) != 220) {
  sesgo_medio(as.numeric(x['Var1']),60,x['Var2'])}
})

df <- data.table(do.call("rbind", params))



final <- merge(encuestas,df, by.x=c("fecha","empresaymedio","partido"), by.y=c("fecha","medio","partido"))


#final$sesgo <- final$intencionvoto - final$consenso #con type en predict =response
final$sesgo <- final$consenso - logit(final$intencionvoto)   #con type en predict =lynk

final <- final[,c('fecha','empresaymedio','sesgo','partido')]

house <- spread(key=partido,value=sesgo, data = final)


###### dibujamos consenso que es nuestro predcit del mÈtodo GAM y predict con type= lynk y logit(intencionvoto)
ggplot(final, aes(x=fecha, y=logit(intencionvoto), col=partido)) +
  geom_jitter(alpha=I(.3), size=I(1.4)) +
  geom_line(aes(y=consenso),data=final) +
  #scale_color_manual(intencionvoto=palette) +
  #scale_fill_manual(intencionvoto=palette) +
  labs(col="", y="Logit IntenciÛn voto predicha (GAM)", x="") +
  guides(colour=guide_legend(override.aes = list(alpha = 1, size=3),
                             direction="horizontal", keywidth=.8)) +
  theme_bw()+theme(legend.position="bottom")


######### calculamos un lm del sesgo por el medio para obtener los intervalos de confianza de esos valores 

modpp <- lm(final[final$partido == "pp",]$sesgo ~ final[final$partido == "pp",]$empresaymedio)
#confint(modpp)

modpsoe <- lm(final[final$partido == "psoe",]$sesgo ~ final[final$partido == "psoe",]$empresaymedio)
#confint(modpsoe)


modcs <- lm(final[final$partido == "cs",]$sesgo ~ final[final$partido == "cs",]$empresaymedio)
#confint(modcs)

modpodemos <- lm(final[final$partido == "podemos",]$sesgo ~ final[final$partido == "podemos",]$empresaymedio)
#confint(modpodemos)


