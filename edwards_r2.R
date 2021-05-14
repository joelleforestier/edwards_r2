# calculate semipartial r2 using the method from Edwards et al., 2008
# 'model' argument takes a lmerTest object
# Joel Le Foresiter
# joel.leforestier@mail.utoronto.ca
# May 14, 2021

edwards_r2 <- function(model) {
  output <- anova(model, type = 3)
  var <- rownames(output)
  R2 <- vector()
  
  for(p in 1:length(output$`F value`)) { # calculate R2 for each predictor in the model
    R2 <- c(R2, round(((output$NumDF[p] / output$DenDF[p]) * output$`F value`[p]) / (1 + ((output$NumDF[p] / output$DenDF[p]) * output$`F value`[p])), 2))
  }
  
  result <- data.frame(var, R2)
  return(result)
}
