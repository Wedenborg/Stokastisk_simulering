### Øvelse 6

A = rep(2,10)
AA = 2
for (j in 1:length(A)){
  A[j] = (AA^j)/factorial(j)
}
xx = 1:10
g = (AA^xx/factorial(xx))/sum(A)
X = vector('numeric', 10)
X[1] = 1
N.iter = 10
n = 10
for (i in 2:N.iter){
  X[-i]=x
  h = sample(c(0,1),1,replace = TRUE, prob = c(0.5,0.5))
  if (h == 1){
    if (x == 10){
      Y = 1
    } else {
      Y = x+1
    }
  } else {
    if (x == 1){
      Y = 10
    } else {
      Y = x+1
    }
  }
  if (g[Y]>=g[x]){
    X[i]=Y
  } else {
    X[i]=sample(c(Y,x),1,replace = TRUE, prob = c(g[Y]/g[x],1-g[Y]/g[x]))

  }
}
