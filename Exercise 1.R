## Exercise 1

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

X  = ceiling(U*10)
hist(X) # Histogrammet ser ud til at være relativt uniform fordelt

## Chi-square test

#U = runif(N,0,1)
#X  = ceiling(U*10)

# Count how many in each class
countX = vector('numeric',10)
for (i in 1:10){
  countX[i] = sum(X==i)
}

TestStatistic = sum((countX - N/10)^2/(N/10))

pValue = 1 - pchisq(TestStatistic, df=10-1-3) # = 2.086298e-06
# The p-value is less than 0.05 and we can reject the hypothesis that the distribution is uniform 


## Correlation test 

library(ggplot2)

ch = vector('numeric',9000)
for (i in 1:9000){ # Udregner for 300 
  Uhead = head(U,-i)
  Utail = tail(U,-i)
  ch[i] = 1/(N-i)* sum(Uhead*Utail)
}
hist(ch)

x = rnorm(9000,0.25,7/(144*N))
chLabel = data.frame(cat='Our',value=ch)
xLabel = data.frame(cat='rnorm',value=x)
data = rbind(chLabel,xLabel)
#data = chLabel
ggplot(data, aes(x=value, colour=cat)) + geom_density()
ggplot(data, aes(x=value, colour=cat)) + geom_density(aes(y=..scaled..))


## Run test 1 (#http://influentialpoints.com/Training/runs_tests.htm)
# Caluculation on our randdom numbers 
n1 = length(U[U < median(U)])
n2 = length(U[U > median(U)])

mu = 2*(n1*n2)/(n1+n2)+1 # = 5001 hvilket er tæt på den forventede mean
var = 2*(n1*n2*(2*n1*n2-n1-n2)) / ((n1+n2)^2*(n1+n2-1))

UU= U
UU[U > median(U)]= 'A'
UU[U < median(U)] = 'B'
r = length(rle(UU)$lengths)

Z = (r-mu)/sqrt(var)
pvalue <- 2 * (1-pt(abs(Z), df=N-1)) # = 0.0003449867 with a signinficans level og 0.05 vi can reject the null hypothesis

# Calculation for the R made
nX1 = length(UX[UX < median(UX)])
nX2 = length(UX[UX > median(UX)])

muX = 2*(nX1*n2)/(nX1+nX2)+1
varX = 2*(nX1*nX2*(2*nX1*nX2-nX1-nX2)) / ((nX1+nX2)^2*(nX1+nX2-1))
UUX= UX
UUX[UX > median(UX)]= 'A'
UUX[UX < median(UX)] = 'B'
r = length(rle(UUX)$lengths)
Z = (r-muX)/sqrt(varX)
pvalue <- 2 * (1-pt(abs(Z), df=N-1)) 
pvalue#  = 0.7489629 We can't reject the null hypothesis

# The easy way to the test runs 
library(randtests)
runs.test(U) 
runs.test(UX)

## Run test 2 up up and down
UpDownRun = vector()

for (i in 2:N){ # Finder ud af hvilket ulighedstegn der er  
  if (U[i] > U[i-1]){
    UpDownRun[i-1] = 'A'
  } else{
    UpDownRun[i-1] = 'B'
  }
  
}
Runs = rle(UpDownRun)$lengths # tæller længden af hvert run
R = vector()
for (i in 1:6){
  R[i]=length(Runs[Runs==i])
}
B = vector('numeric',6)
B = c(1/6,5/24,11/120,19/720,29/5040,1/840)
#B = t(B)
#R= t(R)
A = matrix( 
  c(4529.4, 9044.9, 13568, 18091, 22615, 27892,9044.9,18097,27139,36187,45234,55789,13568,27139,40721,54281,67852,83685,18091,36187,54281,72414,90470,111580,22615,45234,67852,90470,113262,139476,27892,55789,83685,111580,139476,172860), 
  nrow=6, 
  ncol=6)
Z = 1/(N-6)*dot(t((R-N*B)),dot(A,(R-N*B)))  # Regner test statistics
pValue = 1 - pchisq(Z, df=6)  # df er lig med n, fundet i Villy s. 79
## Kolmogorov-Smirnov test 
plot(ecdf(X), verticals=TRUE)
plot(ecdf(U), verticals=TRUE)

Dn = 0
U_ECDF = ecdf(U)
for ( i in 1:length(U)){
    Dn[i] =  abs(U_ECDF(U[i])-U[i])
}
max(Dn)

ks.test(U, 'punif',0,1)

# regn support

## Scatter plot
UScatterHead = head(U,-1)
UScatterTail = tail(U,-1)

plot(UScatterHead,UScatterTail)