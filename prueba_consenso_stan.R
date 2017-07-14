
  # Estimates house effects on the *logit* scale.
  # depends on these objects being in environment:
  # polls, parties
  # Note this is different to house_bias() from a previous post,
  # which drew graphics, and estimated bias on the original scale.
elect_years=2005
pollsters="Roy Morgan"

  houses <- expand.grid(elect_years, pollsters, stringsAsFactors = FALSE)
  names(houses) <- c("ElectionYear", "Pollster")
  
 # for(j in 1:length(parties)){
    the_party = "Labour"
    
    # election results:
    results <- polls %>%
      filter(ElectionYear %in% elect_years & ElectionYear != 2002) %>%
      filter(Pollster == "Election result")  %>%
      filter(Party == the_party) 
    
    
   # for(i in 1:length(elect_years)){
      
      # Note we include *all* pollsters in the data for fitting the model
      thedata <- polls %>%
        filter(ElectionYear == 2005 & Pollster != "Election result") %>%
        filter(Party == the_party)
      
      mod <- gam(VotingIntention ~ s(as.numeric(MidDate)) + Pollster, 
                 family = "quasibinomial", data = thedata)
      
      # for predicting values, we only take the pollsters we have an interest in:
      preddata <- data.frame(MidDate = as.numeric(results[1, "MidDate"]), Pollster = pollsters)
      
      # house effect is shown by the amount the predicted value from polling
      # is *more* than the actual vote.  So a positive score means the poll
      # overestimated the actual vote:
      houseeffects <- predict(mod, newdata = preddata, type = "link") -
        logit(results[1, "VotingIntention"])
      houses[houses$ElectionYear == 2005, the_party] <- houseeffects
    #}
    
  #}   
  
  output <- data_frame(Party = character(), Pollster = character(),
                       Bias = numeric(), SampVar = numeric())
  
  for(i in pollsters){
    for(j in parties){
      disc <- houses[houses$Pollster == i, j]
      #disc <- houses[houses$Pollster == "Roy Morgan", "Labour"]
      print (disc)
      x <- list(disc = disc, N = length(disc))
      fit <- stan(file = 'estimate-house-effects.stan', data = x, control = list(adapt_delta = 0.995))
      fit_e <- rstan::extract(fit)$house_effect
      tmp <- data_frame(
        Party = j,
        Pollster = i,
        Bias = mean(fit_e), 
        SampVar = sd(fit_e) ^ 2)
      output <- rbind(output, tmp)
    }
  }
  output %>%
    arrange(Party, Pollster) %>%

