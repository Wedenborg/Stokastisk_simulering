#### ??velse 6

## Det f??rste!!
A = rep(8,10)
AA = 8
for (j in 0:length(A)){
  A[j] = (AA^j)/factorial(j)
}
X = vector()
xx = 0:10
g = (AA^xx/factorial(xx))/sum(A)
X[1]=1
N.iter = 10000
for (i in 2:N.iter){
  x = X[i-1]
  Y = floor(runif(1,0,11))
  Y
  if (g[Y+1]>=g[x+1]){
    X[i]=Y
  } else {
    X[i]=sample(c(Y,x),1,replace = TRUE, prob = c(g[Y+1]/g[x+1],1-g[Y+1]/g[x+1]))
  }
}
hist(X)
plot(g)

densityX = rle(sort(X))
densityX = densityX[["lengths"]]/N.iter
chisq.test(densityX,g) # p-value = 0.2423


######### i og j
A1 = 4
A2 = 4
ii = 1:5
jj = 1:5
g = matrix(NA, 10, 10)
for (j in 1:10){
  for ( i in 1:10){
    g[i,j] = (A1^i)/factorial(i)*(A2^j)/factorial(j)
  }
}
x =c(1,1)
X = matrix(x,1, 2)
N.iter = 10000
n = 10
for (i in 2:N.iter){
  Y = floor(runif(2,0,11))
  Y
  if (all(g[Y+1]>=g[x+1])){
    x=Y
  } else {
    E = runif(2,0,1)
    if (all(E < g[Y]/g[x])){
      x = Y
    }
  }
  X = rbind(X,x)
}

densityX = vector()

for (i in 1:10){
  for (j in 1:10){
    densityX = c(densityX,length(X[X==c(i,j)]))
  }
}
densityX=densityX/N.iter

# dev.new()
hist(X, freq = FALSE)
chisq.test(densityX,c(g)) # p-value = 0.2504
plot(g)


######### Co-ordinatewise
A1 = 4
A2 = 4
ii = 1:5
jj = 1:5
g = matrix(NA, 10, 10)
for (j in 1:10){
  for ( i in 1:10){
    g[i,j] = (A1^i)/factorial(i)*(A2^j)/factorial(j)
  }
}
x =c(1,1)
X = matrix(x,1, 2)
N.iter = 10000
n = 10
for (i in 2:N.iter){
  Y = floor(runif(2,0,11))
  Y
  if (g[Y+1][1]>=g[x+1][1]){
    x[1]=Y[1]
  } else {
    E = runif(2,0,1)
    if (E[1] < g[Y][1]/g[x][1]){
      x[1] = Y[1]
    }
  }
  if (g[Y+1][2]>=g[x+1][2]){
    x[2]=Y[2]
  } else {
    E = runif(2,0,1)
    if (E[2] < g[Y][2]/g[x][2]){
      x[2] = Y[2]
    }
  }
  X = rbind(X,x)
}

densityX = vector()

for (i in 1:10){
  for (j in 1:10){
    densityX = c(densityX,length(X[X==c(i,j)]))
  }
}
densityX=densityX/N.iter

# dev.new()
hist(X, freq = FALSE)
chisq.test(densityX,c(g)) # p-value = 0.2504
plot(g)


######### GIBBS
A1 = 4
A2 = 4
ii = 1:5
jj = 1:5
g = matrix(NA, 10, 10)
for (j in 1:10){
  for ( i in 1:10){
    g[i,j] = (A1^i)/factorial(i)*(A2^j)/factorial(j)
  }
}

x =c(1,1)
X = matrix(x,1, 2)
N.iter = 10000
n = 10
for (i in 2:N.iter){
  good = FALSE
  while (good == FALSE){
    Y = floor(runif(2,0,11))
    Y
    if (g[Y+1][1]>=g[x+1][1]){
      x[1]=Y[1]
    } else {
      x[1]=x[1]
      }
    if (g[Y+1][2]>=g[x+1][2]){
      x[2]=Y[2]
    } else {
      x[2]=x[2]
    }
    X = rbind(X,x)
    if (dim(X)[1]>3 && x == X[i-1,] && x==X[i-2,] && x==X[i-3,]){
      X = head(X,-2)
      print('hel')
      good = TRUE
    }
  }
}
densityX = vector()

for (i in 1:10){
  for (j in 1:10){
    densityX = c(densityX,length(X[X==c(i,j)]))
  }
}
densityX=densityX/N.iter

# dev.new()
hist(X, freq = FALSE)
chisq.test(densityX,c(g)) # p-value = 0.2504
plot(g)




######### abr dim func
abrmarkov = function(dimension,callType,X0,n){


  dimension = 2
  callType = c(4,4)
  dimension = 2
  X0 =c(3,3)
  n =10

  gFunction = function(cord,callType,dimension){

    g = 1
    for (i in 1:dimension){
    g = g*(callType[i]^(cord[i]))/factorial(cord[i])

    }
    return(g)
  }

  x = X0
  X = matrix(X,1, length(x))
  N.iter = 10000
  for (i in 2:N.iter){
    Y = floor(runif(length(x),0,n+1))
    Y
    if (all( gFunction(Y,callType,dimension)>= gFunction(x,callType,dimension))){
      x=Y
    } else {
      E = runif(length(x),0,1)
      if (all(E < gFunction(Y,callType,dimension)/gFunction(x,callType,dimension))){
        x = Y
      }
    }
    X = rbind(X,x)
  }

  densityX = vector()


  result = X
  return(result)

}


plot(X[,1],X[,3])

X
