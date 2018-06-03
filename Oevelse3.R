# Øvelse 3

# Generate U
N.iter = 10000
X = vector('numeric', N.iter)
U = vector('numeric', N.iter)
M = 10^8
a = 57
c = 1
X[1]=3
for ( i in 1:N.iter){
  X[i+1]=(a*X[i]+c)%%M
  U[i+1]=X[i]/M
}

lambda = 1

# Eksponential fordeling?
FF = 1-exp(lambda*U)

# Invers funktion
TT = -1/lambda*log(1-U)

plot(FF)
plot(TT)
hist(FF)

#Pareto
beta = 1 # Hvad er beta og k??
k = 2.05
Fpar = 1-(beta/U)^k
hist(Fpar)
plot(Fpar)

# Normal

