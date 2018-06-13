# Øvelse 8
n = 10
X = c(56,101, 78, 67, 93, 87, 64,72, 80, 69)
a = -5
b = 5
n = length(X)
X_est = mean(X)
n_boot = 100

subrutine  = function(data,r){
  X_means  = vector('numeric', r)
  X_median = vector('numeric', r)
  n = length(data)
  FF = ecdf(data)
  for (i in 1:r){
    Samples = sample(data, size = n,replace = TRUE, prob = FF(data) )
    X_means[i] = mean(Samples)
    X_median[i] = median(Samples)
  }
  med = mean(X_median)
  gns = mean(X_means)
  var_mean = var(X_means)
  var_median = var(X_median)
  return(c(med, gns,var_mean,var_median))
}

U = runif(20000,0,1)
beta = 1
k = 1.05
X_par = beta*(U^(-1/k))
X_par = X_par[is.finite(X_par)]
X_par = X_par[!is.na(X_par)]


mean(X_par)

median(X_par)
