NBspie <- function(outcomesBen, outcomesHarm, weightsBen, weightsHarm, labelsBen, labelsHarm, lambda = seq(0, 1, by=0.05)) {
  
  ### FUNCTION ARGUMENTS:
  # outcomesBen = (vector of) of benefits outcomes measures with treatments as row names; must be between 0-1
  # outcomesHarm = (vector of) of harms/acceptability outcomes measures with treatments as row names; must be between 0-1
  # weightsBen = (vector of) weights for the benefits outcomes, same length as outcomesBen; must add up to 1
  # weightsHarm = (vector of) weights for the harms/acceptability outcomes, same length as outcomesHarm; must add up to 1
  # labelsBen = string vector of outcome labels for the benefits outcomes,  same length as outcomesBen
  # labelsHArm = string vector of outcome labels for the harms/acceptability outcomes, same length as outcomesHarm
  # lambda = (vector of) trade-off values needed for the calculation of net benefit; values should be between 0 and 1
  
  
  
  # calculation of standardised area within the benefits spie chart for each treatment (SAWIS_B)
  pos <- t(sapply(rownames(outcomesBen) , function(i) spie.chart.B(as.numeric(outcomesBen[i,]),theta=weightsBen*2*pi,outcome.label = labelsBen)))
  colnames(pos) <- c("chart", "area")
  
  # calculation of standardised area within the harms spie chart for each treatment (SAWIS_H)
  neg <- t(sapply(rownames(outcomesHarm) , function(i) spie.chart.H(as.numeric(outcomesHarm[i,]),theta=weightsHarm*2*pi,outcome.label = labelsHarm)))
  colnames(neg) <- c("chart", "area")
  

  # calculation of net-benefit standardised area within the spie chart for each treatment (SAWIS_NB)
  NB <- array(data = NA, dim = c(length(lambda), nrow(outcomesBen)), dimnames = list(lambda, rownames(outcomesBen)))
  for (i in 1:length(lambda)) {
    NB[i,] <- as.numeric(gsub("Area inside spie chart = ", "", pos[,"area"])) - (lambda[i]*as.numeric(gsub("Area inside spie chart = ", "", neg[,"area"])))
  }

  return(list(SPIEchart_Ben=pos, SPIEchart_Harm=neg, NetBenefit=NB))
  
}


# to show values of SAWIS_NB for all treatment across all values of lambda run: NBspie$NetBenefit
# this can then be used to produce plots