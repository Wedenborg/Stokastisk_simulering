### OEVELSE 2

U=runif(10000,0,1)
X = (log(U)/log(1-1/3))+1
hist(X)



# crude
p = c(7/48,5/48,1/8,1/16,1/4,5/16)
X_crude = vector('numeric',6)
p1 = sum(p[1])
p2 = sum(p[1:2])
p3= sum(p[1:3])
p4 = sum(p[1:4])
p5 = sum(p[1:5])
p6 = sum(p[1:6])


for ( i in 1:length(U)){
  if (U[i]<=p1){
    X_crude[i]=1
  } else if (U[i]<=p2){
    X_crude[i]=2
  } else if (U[i]<=p3){
    X_crude[i]=3
  } else if (U[i]<=p4){
    X_crude[i]=4
  } else if (U[i]<=p5){
    X_crude[i]=5
  } else if (U[i]<=p6){
    X_crude[i]=6
  }
}
hist(X_crude)
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
  FF[k] = FF[k]-(1-FF[j])
  if (FF[k]<1){
    G = G[!G %in% k]
    S = c(S,k)
  } else {
    S = S[!S %in% j]
  }
}
print(FF)
print(L)

# Rejection method

X = vector()
Y = ceiling(runif(10000,0,6))
U = runif(10000,0,1)

q = c(1/6,1/6,1/6,1/6,1/6,1/6)
C = max (p/q)

for (i in 1:10000){
  J = Y[i]
  if (U[i] < p[J]/(q[J]*C)){
    X = c(X,Y[i])
  }
}
Freq_of_output = length(X)/10000

X_Rejection = X
hist(X_Rejection)


### Test if Crude and Rejection are the same

t.test(X_crude,X_Rejection) # we can't reject H_0
