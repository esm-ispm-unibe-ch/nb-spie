NBspie <- function(outcomesBen, outcomesHarm, weightsBen, weightsHarm, labelsBen, labelsHarm, lambda = seq(0, 1, by=0.05)) {
  
  ### FUNCTION ARGUMENTS:
  # outcomesBen = array of beneficial outcomes with treatments as row names; must be between 0-1
  # outcomesHarm = array of harmful outcomes with treatments as row names; must be between 0-1
  # weightsBen = (vector of) weights for the beneficial outcomes; must add up to 1
  # weightsHarm = (vector of) weights for the harmful outcomes; must add up to 1
  # labelsBen = string vector of outcome labels for the beneficial outcomes
  # labelsHArm = string vector of outcome labels for the harmful outcomes
  # lambda = (vector of) threshold values needed for the calculation of net benefit; values should be between 0 and 1
  
  pos <- t(sapply(rownames(outcomesBen) , function(i) spie.chart.B(as.numeric(outcomesBen[i,]),theta=weightsBen*2*pi,outcome.label = labelsBen)))
  colnames(pos) <- c("chart", "area")
  
  neg <- t(sapply(rownames(outcomesHarm) , function(i) spie.chart.H(as.numeric(outcomesHarm[i,]),theta=weightsHarm*2*pi,outcome.label = labelsHarm)))
  colnames(neg) <- c("chart", "area")
  

  NB <- array(data = NA, dim = c(length(lambda), nrow(outcomesBen)), dimnames = list(lambda, rownames(outcomesBen)))
  for (i in 1:length(lambda)) {
    NB[i,] <- as.numeric(gsub("Area inside spie chart = ", "", pos[,"area"])) - (lambda[i]*as.numeric(gsub("Area inside spie chart = ", "", neg[,"area"])))
  }

  return(list(SPIEchart_Ben=pos, SPIEchart_Harm=neg, NetBenefit=NB))
  
}


