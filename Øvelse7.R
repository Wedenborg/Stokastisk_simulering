#### Oevelse 7

inputM = matrix(c(0,sqrt(2),2,sqrt(2), sqrt(2),0,sqrt(2),2,2,sqrt(2),0,sqrt(2),sqrt(2),2,sqrt(2),0),4,4)

cost = vector()
X0 = c(1,3,2,4,1)
costfunk = function(input,X){
  # Input = matrix with cost
  # X = startgæt
  cost = vector()
  N.iter = dim(input)[1]
  for (i in 1:N.iter){
    cost[i] = input[X[i],X[i+1]]
  }
  sumcost = sum(cost)
  return(sumcost)
}


annealingfunk = function(k, X,input){
  N = 10
  sti = matrix(X,1,length(X))
  for (i in 1:N){
    k = i
    X_trim = tail(head(X,-1),-1)
    ombyt = sample(2:(length(X_trim)+1), size = 2,replace = FALSE, prob = NULL )
    ombyt
        X_test = X
    X1 = X[ombyt[1]]
    X2 = X[ombyt[2]]
    X_test[ombyt[1]]=X2
    X_test[ombyt[2]]=X1
    TT = 1/sqrt(1+k)
    if (costfunk(input,X_test)<costfunk(input,X)){
      X = X_test
    } else if (exp((-costfunk(input,X_test)-costfunk(input,X))/TT) < runif(1,0,1)) {
      X = X_test
    }
    sti = rbind(sti,X)
  }
  return(sti)
}

