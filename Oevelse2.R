#??VELSE 2
# Implementing LCG
N = 10000
UX = runif(N,0,1)
X = vector('numeric',N)
U =  vector('numeric',N)
X[1] = 3#2
a =110
c = 11
M = 1637
U[1] = X[1]/M
for (i in 2:N){
  X[i] =  (a*X[i-1]+c)%%M
  U[i] = X[i]/M
}


-

# crude
p = c(7/48,5/48,1/8,1/16,1/4,5/16)
X = vector('numeric',6)
p1 = sum(p[1])
p2 = sum(p[1:2])
p3= sum(p[1:3])
p4 = sum(p[1:4])
p5 = sum(p[1:5])
p6 = sum(p[1:6])


for ( i in 1:length(U)){
  if (U[i]<=p1){
    X[i]=1
  } else if (U[i]<=p2){
    X[i]=2
  } else if (U[i]<=p3){
    X[i]=3
  } else if (U[i]<=p4){
    X[i]=4
  } else if (U[i]<=p5){
    X[i]=5
  } else if (U[i]<=p6){
    X[i]=6
  }
}
hist(X)
# regn de funde % ud 

## Alias method
FF = (6)*p
L = vector('numeric',6)
G = vector()
S = vector()
L = 1:length(L)

for ( i in 1:length(FF)){
  if (FF[i]>=1) {
    G[i] = i
  } else {
    S[i] = i
  }
}
G = G[!is.na(G)]
S = S[!is.na(S)]

while (length(S)>0){
  k = G[1]
  j = S[1]
  L[j] = k
  FF[k] = k-(1-j)
  if (FF[k]<1){
    G = G[!G %in% 1]
    S = c(S,k)
  } else {
    S = S[!S %in% j]
  }
}

# Rejection method
pt(0.1,1)
