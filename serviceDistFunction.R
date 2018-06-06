serviceDistFunction = function (serviceDist, meanServiceTime){
  
  good = F
  
  while (good == F) {
    U = runif(1,0,1)
    if (serviceDist == 'Exponential'){
      lambda = 1/meanServiceTime
      X_service = -log(U)/lambda
    }
    
    else if (serviceDist == 'Perato'){
      beta = .5121951221*meanServiceTime # Hvad er beta og k??
      k = 2.05
      X_service = beta*(U^(-1/k))
    }
    
    else if (serviceDist == 'Constant'){
      X_service = meanServiceTime
    }
    
    else if (serviceDist=='Normal'){
      X_service = (-2*log(U))^(1/2)*cos(2*pi*runif(1,0,1))+meanServiceTime
    }
    
    if (is.finite(X_service) || !is.na(X_service) || X_service >=0){
      good = T
    }
  }
  return(X_service)
}
