#Øvelse 1
#Skriv et program, der genererer 10000 random
##pseudo numbers og viser den i et histogram med
###med 10 klasser


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
hist(U,breaks = 10)


n1=length(U[median(U)<U])
n2=length(U[median(U)>U])
library(randtests)
gns=2*(n1*n2)/(n1+n2)+1
vari=2*((n1*n2*(2*n1*n2-n1-n2))/((n1+n2)^2*(n1+n2-1)))

runs.test(U, alternative = c("two.sided"))

