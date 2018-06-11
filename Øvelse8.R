# Øvelse 8
n = 10
X = c(56,101, 78, 67, 93, 87, 64,72, 80, 69)
a = -5
b = 5

X_est = mean(X)
X_means = vector('numeric', 100)
for (i in 1:100){
  Samples = sample(X, size = n,replace = TRUE, prob = NULL )
  X_means[i] = mean(Samples)
}
var(X_means)
