##### Øvelse 5

### Crude Monte Carlo
library(ggplot2)
U = runif(100,0,1)
X_crude = exp(U)
X_bar = mean(X_crude)
sd(X_crude)
n = length(X_crude)

Conf1 = mean(X_crude) - qt(0.975, df =n-1) * sd(X_crude) / sqrt(n)
Conf2 = mean(X_crude) + qt(0.975, df =n-1) * sd(X_crude) / sqrt(n)

plot(X_bar)

### Antithetic variables
X_anti = (exp(U)+exp(1)/exp(U))/2
X_antibar = mean(X_anti)


### Control
X = exp(U)
Y = U
muY = 1/2 # se eksempel i slides
Xb = mean(X)
Yb = mean(Y)
cs = -sum((X-Xb)*(Y-Yb))/sum((Y-Yb)^2)
Z = X_control+ cs*(Y-muY)
X_controlbar=mean(Z)
### Stratified

W = (exp())



