#ØVELSE 2

U = runif(n = 10000,0,1)
#U = rgeom(10000,0.3)
hist(U)

# crude
p = c(7/48,5/48,1/8,1/16,1/4,5/16)
X = vector('numeric',6)
p1 = sum(p[1])
p2 = sum(p[1:2])
p3= sum(p[1:3])
p4 = sum(p[1:4])
p5 = sum(p[1:5])
p6 = sum(p[1:6])


for ( i in 1:length(U)){
  if (U[i]<=p1){
    X[i]=1
  } else if (U[i]<=p2){
    X[i]=2
  } else if (U[i]<=p3){
    X[i]=3
  } else if (U[i]<=p4){
    X[i]=4
  } else if (U[i]<=p5){
    X[i]=5
  } else if (U[i]<=p6){
    X[i]=6
  }
}
hist(X)
