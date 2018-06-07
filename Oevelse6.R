#### Øvelse 6
#
# A = rep(8,10)
# AA = 8
# for (j in 0:length(A)){
#   A[j] = (AA^j)/factorial(j)
# }
#
# xx = 0:10
# g = (AA^xx/factorial(xx))/sum(A)

A1 = 4
A2 = 4
ii = 1:5
jj = 1:5
g = matrix(NA, 10, 10)
for (j in 1:10){
  for ( i in 1:10){
    g[i,j] = (A1^i)/factorial(i)*(A2^i)/factorial(j)
  }
}
x =c(1,1)
X = matrix(x,1, 2)
N.iter = 10000
n = 10
for (i in 2:N.iter){
  Y = floor(runif(2,0,11))
  Y
  if (all(g[Y+1]>=g[x+1])){
    x=Y
  } else {
    E = runif(2,0,1)
    if (all(E < g[Y]/g[x])){
      x = Y
    }
  }
  X = rbind(X,x)
}



dev.new()
hist(X, freq = FALSE)

plot(g)
