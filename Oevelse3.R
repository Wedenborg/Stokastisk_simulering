# Oevelse 3

###Eksponential fordeling?
U = runif(10000,0,1)
lambda = 1
FF = 1-exp(lambda*U)
X_exp = -log(U)/lambda
X_exp = X_exp[is.finite(X_exp)]
hist(X_exp)




###Pareto
U = runif(10000,0,1)
beta = 1
k = 4
X_par = beta*(U^(-1/k))
X_par = X_par[is.finite(X_par)]
X_par = X_par[!is.na(X_par)]
Fpar = 1-(beta/X_par)^k
hist(X_par)

## Sammenling mean og var
### med udregnet forventet værdi og var
mean(X_par)
var(X_par)
E = beta*(k/(k-1))
E
vari = beta^2*k/((k-1)^2*(k-2))
vari

# Mean og forventet mean er n??sten ens
# Variance afh??nger af k og kommer t??ttere p?? hinanden som k stiger


plot(ecdf(X_par), verticals=TRUE,xlim=c(1,5))


### Normal distribution
####side 85 i pdf
U = runif(10000,0,1)

TT = vector('numeric', length(U))
TTT = vector('numeric', length(U))

for ( i in 1:length(U)){
TT[i] = (-2*log(U[i]))^(1/2)*cos(2*pi*U[i+1])
TTT[i] = (-2*log(U[i]))^(1/2)*sin(2*pi*U[i+1])
}
X_norm = as.vector(rbind(TT,TTT))
X_norm = X_norm[is.finite(X_norm)]
hist(X_norm)


### Test normal!
n <- length(X_norm)
## Compute the tobs - the observed test statistic
tobs <- (mean(X_norm)) / (sd(X_norm) / sqrt(n))
tobs
## Compute the p-value as a tail-probability in the t-distribution
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue # No evidence AGAINST hypotesen = Den er nok normalfordelt

### KS test for normal
dev.new()
library(vcd)
library(MASS)
library(EnvStats)
# estimate the parameters
ks.test(X_norm,"pnorm",mean(X_norm),sd(X_norm))
hist(X_norm, freq = FALSE, breaks = 100, xlim = c(quantile(X_norm, 0.01), quantile(X_norm, 0.99)))
epdfPlot(x = X_norm,add = TRUE,epdf.col = 'red')

### KS test for exponential!!
# estimate the parameters
fit1 <- fitdistr(X_exp, "exponential")
# goodness of fit test exp
ks.test(X_exp, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused
# plot a graph
hist(X_exp, freq = FALSE, breaks = 100, xlim = c(0, quantile(X_exp, 0.99)))
curve(dexp(X_exp, rate = fit1$estimate),xname='X_exp', xlim=c(0.1, quantile(X_exp, 0.99)),col = "red", add = TRUE)



### KS Test Pareto
library(rmutil)
# goodness of fit test exp
ks.test(X_par, "ppareto", 4,min(X_par)) # p-value > 0.05 -> distribution not refused

# plot a graph
hist(X_par, freq = FALSE, breaks = 100, xlim = c(min(X_par), quantile(X_par, 0.99)))
curve(dpareto(X_par, 3.2,1),xname='X_par', xlim=c(min(X_par), quantile(X_par, 0.99)),col = "red", add = TRUE)


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
