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

XX = -log(U)/lambda
XX = XX[is.finite(XX)]

plot(FF)
plot(TT)
hist(FF)
hist(XX)
curve(dexp(XX), xname="XX", add=TRUE, col="red")
#Pareto

beta = 1 # Hvad er beta og k??
k = 3
x = beta*(U^(-1/k))
Fpar = 1-(1+(beta/x))^k
hist(Fpar)
plot(Fpar)
mean(Fpar)
var(Fpar)

mean(Fpar)
var(Fpar)

E = beta*(k/(k-1))
E

vari = beta^2*k/((k-1)^2*(k-2))
vari
# Normal side 85 i pdf

TT = vector('numeric', length(U))
TTT = vector('numeric', length(U))

for ( i in 1:length(U)){
TT[i] = (-2*log(U[i]))^(1/2)*cos(2*pi*U[i+1])
TTT[i] = (-2*log(U[i]))^(1/2)*cos(2*pi*U[i+1])
}
TFinal = as.vector(rbind(TT,TTT))
TFinal = TFinal[is.finite(TFinal)]
hist(TFinal)


# Test normal!
n <- length(TFinal)
## Compute the tobs - the observed test statistic
tobs <- (mean(TFinal)) / (sd(TFinal) / sqrt(n))
tobs

## Compute the p-value as a tail-probability in the t-distribution
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue # No evidence AGAINST hypotesen = Den er nok normalfordelt


# Test exponential

## Sum up to get the running time
xCum <- cumsum(XX)
## Use the hist function to count in intervals between the breaks,
## here 0,1,2,...
tmp <- hist(xCum, breaks=0:ceiling(max(xCum)))
## Plot the discrete empirical pdf
plot(table(tmp$counts)/length(tmp$counts))
## Add the Poisson pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")

#Den røde er en poission og det sorte er de generede tal! Det ses at den følger perfekt!!

