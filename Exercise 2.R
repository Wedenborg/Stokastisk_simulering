# Exercise 2
## 1)
G = rgeom(10000,0.7) # Make a Geometric Distribution
hist(U)

## 2)
## six-point distribution 
# direct (crude) methode
U = runif(10000,0,1)
p = c(7/48,5/48,1/8,1/16,1/4,5/16)
p1 =sum(p[1])
p2 = sum(p[1:2])
p3 = sum(p[1:3])
p4 = sum(p[1:4])
p5 = sum(p[1:5])
p6 = sum(p[1:6])

for (i in 1:N){
  if (U[i] <= p1){
    X[i] = 1
  } else if (U[i] <= p2){
    X[i] = 2
  }else if (U[i] <= p3){
    X[i] = 3
  }else if (U[i] <= p4){
    X[i] = 4
  }else if (U[i] <= p5){
    X[i] = 5
  }else if (U[i] <= p6){
    X[i] = 6
  }
}
hist(X)




#Alias
n  = 6
p = c(7/48,5/48,1/8,1/16,1/4,5/16)
L = c(1,2,3,4,5,6)

F = n*p
G = F[F>=1]
S = F[F<=1]

while (sum(S)>0){
  k = G[1]
  j = S[1]
  L[j] = k
  F[k] = F[k]-(1-F[j])
  if (F[k] <1-eps){
    G[1]=0
    S= c(S,k)
  }
  S[1] = 0
}
