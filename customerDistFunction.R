customerDistFunction = function (customerDist, meanCustomerTime){
  good = F
  
  while (good == F) {
    
    U = runif(1,0,1)
    
    if (customerDist == 'Exponential'){
      lambda = 1/meanCustomerTime
      X_customer = -log(U)/lambda
    }
    
    else if (customerDist == 'Erlang'){ #Erlang 8
      lambda = 8/meanCustomerTime
      U = runif(8,0,1)
      X_customer = -1/lambda*log(prod(U))
    }
    
    else if(customerDist =='Hyper'){
      lambda  = 1/sample(c(0.8333, 5), 1, replace = TRUE,prob = c(0.8,0.2))
      X_customer = -log(U)/lambda
    }
    
    if (is.finite(X_service) || !is.na(X_service) || X_service >=0){
      good = T
    }
  }
  
  return(X_customer) 
}