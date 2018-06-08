#### Oevelse 7

input = matrix(c(0,sqrt(2),2,sqrt(2), sqrt(2),0,sqrt(2),2,2,sqrt(2),0,sqrt(2),sqrt(2),2,sqrt(2),0),4,4)

A = rep(8,10)
AA = 8
for (j in 0:length(A)){
  A[j] = (AA^j)/factorial(j)
}


X = vector()
xx = 0:10

X[1]=1
N.iter = 10000
randomWalk = function(inputM,x)
  good = FALSE
  while(good = TRUE){
    Y = floor(runif(1,0,size(inputM[1]+1))
    if (g[Y+1]>=g[x+1]){
      X[i]=Y
    } else {
      X[i]=sample(c(Y,x),1,replace = TRUE, prob = c(g[Y+1]/g[x+1],1-g[Y+1]/g[x+1]))
    }
  }
