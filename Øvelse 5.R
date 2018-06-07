##### Øvelse 5

### Crude Monte Carlo
U = runif(100,0,1)
X_crude = exp(U)
X_crudebar = mean(X_crude)
sd(X_crude)
n = length(X_crude)

Conf_crude1 = mean(X_crude) - qt(0.975, df =n-1) * sd(X_crude) / sqrt(n)
Conf_crude2 = mean(X_crude) + qt(0.975, df =n-1) * sd(X_crude) / sqrt(n)


### Antithetic variables
X_anti = (exp(U)+exp(1)/exp(U))/2
X_antibar = mean(X_anti)

Conf_anti1 = mean(X_anti) - qt(0.975, df =n-1) * sd(X_anti) / sqrt(n)
Conf_anti2 = mean(X_anti) + qt(0.975, df =n-1) * sd(X_anti) / sqrt(n)

### Control
X = exp(U)
Y = U
muY = 1/2 # se eksempel i slides
Xb = mean(X)
Yb = mean(Y)
cs = -sum((X-Xb)*(Y-Yb))/sum((Y-Yb)^2)
Z = X+ cs*(Y-muY)
X_controlbar=mean(Z)

Conf_control1 = mean(Z) - qt(0.975, df =n-1) * sd(Z) / sqrt(n)
Conf_control2 = mean(Z) + qt(0.975, df =n-1) * sd(Z) / sqrt(n)


### Stratified
strata = 100 #gæt
n = strata
UU = matrix(U,10,10)
W = vector()
W[1] = U[1]/strata
for (i in 2: strata){
  W[i] = exp((i-1)/strata+U[i]/strata)
}
X_strati = sum(W)/strata
Conf_strati1 = mean(W) - qt(0.975, df =n-1) * sd(W) / sqrt(n)
Conf_strati2 = mean(W) + qt(0.975, df =n-1) * sd(W) / sqrt(n)


### Plot med CI
dev.new()

punkter = c(X_crudebar,X_antibar,X_controlbar, X_strati)
Nedre_conf = c(Conf_crude1,Conf_anti1,Conf_control1,Conf_strati1)
Øvre_conf = c(Conf_crude2,Conf_anti2,Conf_control2,Conf_strati2)
df=data.frame(x = 1:4,
              FF = punkter,
              L = Nedre_conf,
              U = Øvre_conf)

library(ggplot2)
ggplot(df, aes(x = x, y = FF)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))
