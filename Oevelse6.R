### Øvelse 6

A = rep(8,10)
AA = 8
for (j in 0:length(A)){
  A[j] = (AA^j)/factorial(j)
}

xx = 0:10
g = (AA^xx/factorial(xx))/sum(A)
X = vector('numeric', 10)
X[1] = 1
N.iter = 10000
n = 10
for (i in 2:N.iter){
  x = X[i-1]
  Y = floor(runif(1,0,11))
  Y
  if (g[Y+1]>=g[x+1]){
    X[i]=Y
  } else {
    X[i]=sample(c(Y,x),1,replace = TRUE, prob = c(g[Y+1]/g[x+1],1-g[Y+1]/g[x+1]))

  }
}
hist(X)
plot(g)
