# Oevelse 3

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
curve(dexp(10000,lambda), xname="XX", add=TRUE, col="red")




###Pareto

beta = 1 # Hvad er beta og k??
k = 4
x = beta*(U^(-1/k))
x = x[is.finite(x)]
x = x[!is.na(x)]
Fpar = 1-(1+(beta/x))^k
hist(Fpar)
plot(Fpar)
mean(Fpar)
var(Fpar)

mean(x)
var(x)

E = beta*(k/(k-1))
E

vari = beta^2*k/((k-1)^2*(k-2))
vari

# Mean og forventet mean er n??sten ens
# Variance afh??nger af k og kommer t??ttere p?? hinanden som k stiger


plot(ecdf(x), verticals=TRUE,xlim=c(1,5))
### Normal side 85 i pdf

TT = vector('numeric', length(U))
TTT = vector('numeric', length(U))

for ( i in 1:length(U)){
TT[i] = (-2*log(U[i]))^(1/2)*cos(2*pi*U[i+1])
TTT[i] = (-2*log(U[i]))^(1/2)*sin(2*pi*U[i+1])
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


### Test exponential

## Sum up to get the running time
xCum <- cumsum(XX)
## Use the hist function to count in intervals between the breaks,
## here 0,1,2,...
tmp <- hist(xCum, breaks=0:ceiling(max(xCum)))
## Plot the discrete empirical pdf
plot(table(tmp$counts)/length(tmp$counts))
## Add the Poisson pdf to the plot
lines(0:20, dpois(0:20,lambda), type="h", col="red")



library(vcd)
library(MASS)





# estimate the parameters
fit1 <- fitdistr(XX, "exponential") 


# goodness of fit test exp
ks.test(XX, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused


# plot a graph
hist(XX, freq = FALSE, breaks = 100, xlim = c(0, quantile(ex, 0.99)))
curve(dexp(XX, rate = fit1$estimate),xname='XX', xlim=c(0.1, quantile(ex, 0.99)),col = "red", add = TRUE)



#Den roede er en poission og det sorte er de generede tal! Det ses at den foelger perfekt!!


### Test Pareto

# distribution, cdf, quantile and random functions for Pareto distributions
dpareto <- function(x, xm, alpha) ifelse(x > xm , alpha*xm**alpha/(x**(alpha+1)), 0)
ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm/q)**alpha, 0 )
qpareto <- function(p, xm, alpha) ifelse(p < 0 | p > 1, NaN, xm*(1-p)**(-1/alpha))
rpareto <- function(n, xm, alpha) qpareto(runif(n), xm, alpha)

# estimate the parameters
fit1 <- fitdistr(x, "pareto") 


# goodness of fit test exp
ks.test(Fpar, "ppareto", fit1$estimate) # p-value > 0.05 -> distribution not refused


# plot a graph
hist(Fpar, freq = FALSE, breaks = 100, xlim = c(0, quantile(ex, 0.99)))
curve(dexp(Fpar, rate = fit1$estimate),xname='Fpar', xlim=c(0.1, quantile(ex, 0.99)),col = "red", add = TRUE)


## Sum up to get the running time
xCum <- cumsum(x)
## Use the hist function to count in intervals between the breaks,
## here 0,1,2,...
tmp <- hist(xCum, breaks=0:ceiling(max(xCum)))
## Plot the discrete empirical pdf
plot(table(tmp$counts)/length(tmp$counts))
## Add the Poisson pdf to the plot
lines(0:20, dpareto(0:20,min(x),3), type="h", col="red")


##########
Conf = matrix( 
  nrow=100, 
  ncol=2)
for (i in 1:100){
  b = a+10
  sample = TFinal[a:b]
  a= i*10
  n = length(sample)

  
  Conf[i,1] = mean(sample) - qt(0.975, df =n-1) * sd(sample) / sqrt(n)
  Conf[i,2] = mean(sample) + qt(0.975, df =n-1) * sd(sample) / sqrt(n)
}

plot(Conf[,1],col = 'red', type='l' , ylim=c(-1.5,1.5))
lines(Conf[,2],col = 'blue')

#   Conf aendre sig meget men for der meste er 0 indenfor.
