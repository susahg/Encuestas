tmp <- final[final$partido == "pp",]

tmp$sesgo <- 100 * tmp$sesgo

modelo <- lm(sesgo ~ -1 + empresaymedio, data = tmp)

tmp <- final[final$partido == "pp",]
modelo <- lm(intencionvoto ~ -1 + consenso + empresaymedio, data = tmp)
predict(modelo, interval = "predict")

kk <- cbind(tmp, predict(modelo, interval = "predict"))

kk$lower <- 100 * kk$lwr
kk$upper <- 100 * kk$upr
kk$pred  <- 100 * kk$fit
kk$intencionvoto <- 100 * kk$intencionvoto


kk[, c("empresaymedio", "lower", "intencionvoto", "pred",  "upper")]



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
  labs(x = "", y = "", colour = "", caption = "")  +
  ggtitle("fatal","")


kk[, c("empresaymedio", )]


kk[, -(1:2)]

kk[, c("empresaymedio", "intencionvoto", "lwr", "upr")]


plot(kk$intencionvoto, 100 * kk$fit)

debug(predi_intencion_voto)
predi_intencion_voto(Sys.Date(), final, 60, df)




