##### ??velse 5

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
strata = 100 #g??t
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
??vre_conf = c(Conf_crude2,Conf_anti2,Conf_control2,Conf_strati2)
df=data.frame(x = 1:4,
              FF = punkter,
              L = Nedre_conf,
              U = ??vre_conf)

library(ggplot2)
ggplot(df, aes(x = x, y = FF)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))


#################
## Control 


# Modeficeret s?? den tager contril variates metode. Vi bergner c ved at benytter customer 1000 til 6000.
# Vores Y er det tal fra den uniform distribution der benyttes til at berengen customerDist. Y har en mean p?? 0.5


CustomerSimulation = function(customerDist, meanCustomerTime,serviceDist, meanServiceTime,ServiceUnits){
  
  # customerDist must be one of following: Exponential, Erlang or Hyper
  # serviceDist must be : Exponential, Perato, Constant or Normal
  
  # Example:  CustomerSimulation('Exponential',1,'Exponential',8,10)
  
  ### Load Functions
  
  
  
  setwd("~/Documents/GitHub/Stokastisk_simulering")
  source('serviceDistFunction.R')
  source('customerDistFunction.R')
  
  
  
  
  indkoersel = 6000 # number og customers before logging 
  N = 10000 # Number of customer logging
  
  a = 0
  
  RejectionRate = vector()
  for (i in 1:10){
    B = 0
    Customers = 0
    S = vector('numeric',ServiceUnits)
    clock = 1
    Next_customer = 1
    
    blocklog = vector()
    uniformLog = vector()
    customerDistFunctionLog = vector()
    c = 0
    
    
    while (Customers < N+indkoersel){
      a=a+1
      if(Customers == indkoersel){ # Indkoersel
        Start_customer = Customers
        Start_B = B
      }
      
      if (Customers == 6000){
        c = -sum(customerDistFunctionLog-mean(customerDistFunctionLog)*uniformLog-mean(uniformLog))/sum((uniformLog-mean(uniformLog))^2)
      }
      
      if (any(S == clock)){ # er svare clock til at i kunder er serviceret
        S[S==clock] = 0 # vi laver en tom plads
        
      }
      
      if (Next_customer == clock){ # svare event til at der kommer en ny kunde
        Customers = Customers + 1
        sampleNumber =customerDistFunction(customerDist, meanCustomerTime)[1]
        sampleUniform = customerDistFunction(customerDist, meanCustomerTime)[2]
        Next_customer = clock + sampleNumber + c*(sampleUniform-1/2)  # Tiden for hvorn??r n??ste kunde kommer beregnes
        if (Customers > 1000){
          customerDistFunctionLog = c(customerDistFunctionLog,sampleNumber)
          uniformLog = c(uniformLog,sampleUniform)
        }
        if (any(S == 0)){ # Hvis der er en tom plads beregnes tiden for hvor langt tid det tager at ekpiderer kunder der tager denne tomme plads
          S[S==0][1] =  serviceDistFunction(serviceDist, meanServiceTime) + clock
        }
        else { # hvis der ikke er en tom plads bliver kunden blockerer og ender i B
          B = B+1
        }
      }
      
      
      if(sum(S)==0){
        clock = Next_customer
      }
      else if (min(S[S!=0])< Next_customer){ # Vi s??tter clock til n??ste gang der kommer en event.
        clock =  min(S[S!=0])
      } else{
        clock =  Next_customer
      }
      
    }
    RejectionRate[i] = (B-Start_B)/(Customers-Start_customer)
  }
  
  
  avg = mean(RejectionRate)
  conf_under =avg - qt(0.975, df = 9) * sd(RejectionRate) / sqrt(Customers-Start_customer)
  
  conf_above = avg + qt(0.975, df = 9) * sd(RejectionRate) / sqrt(Customers-Start_customer)
  output = c(avg,conf_under,conf_above )
  
  return(print(output))
}




