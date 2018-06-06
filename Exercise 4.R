### EXercise 4
###
### Noter:
### Simulationen skal k??rer lidt tid f??r vi begynder at m??le.
### Hvordan passer Rejection Rate med den teoretiske, som er opgivet p?? sidste slide
###
### Benyt den eksponentiele fordeling vi selv har lavet.
###
### Det st??r vi skal lave et program der tager offered traffic??? og antal service unit som input
### Output for denne funktion skal formegentligt v??re rejection rate

 traffic =1/8
 ServiceUnits = 10
 # beta =4.097560976
 # traffic = beta


# CustomerSimulation = function(traffic,ServiceUnits){
  # Generate U
  

  
  lambda = traffic
  X_service = -log(U)/lambda
  
  lambda = 1
  X_customer = -log(U)/lambda

  
  
  ######################
  #Choose your distribution
  
  ## Exponential
  lambda = 1
  X_customer = -log(U)/lambda

  
  # ## Erlang
  # lambda = 1
  # k= 1
  # D = length(U)/k
  # a = 1
  # X_customer = vector()
  # for (i in 1:D){
  #   b = a+k-1
  #   X_customer[i] = -1/lambda*log(prod(U[a:b]))
  #   a = i*k+1
  # }
  
  # ## Hyper exponential
  # lambda1 = 0.8333
  # lambda2 = 5.0
  # prob1 = 0.8
  # prob2 = 0.2
  # 
  # X_customer = prob1*(-log(U)/lambda1)+prob2*(-log(U)/lambda2)


  ## Pareto
  # beta = traffic # Hvad er beta og k??
  # k = 2.05
  # X_service = beta*(U^(-1/k))
  # Der n??r ikke at blive blockeret nogle
  
  
  ## Constant service time 
   # X_service = c(rep(8, length(U)))
  
  # ## Normal dist
  # TT = vector('numeric', length(U))
  # TTT = vector('numeric', length(U))
  # 
  # for ( i in 1:length(U)){
  #   TT[i] = (-2*log(U[i]))^(1/2)*cos(2*pi*U[i+1])
  #   TTT[i] = (-2*log(U[i]))^(1/2)*sin(2*pi*U[i+1])
  # }
  # X_service = as.vector(rbind(TT,TTT))+8
  # X_service
  # 
  
  ##
  X_customer = X_customer[is.finite(X_customer)]
  X_customer = X_customer[!is.na(X_customer)]
  X_service = X_service[is.finite(X_service)]
  X_service = X_service[!is.na(X_service)]
  ##
  
  
  
  ###
  
  
  indkoersel = 6000
  
  N = 10000
  a = 0
  RejectionRate = vector()
  for (i in 1:10){
    B = 0
    Customers = 0
    S = vector('numeric',ServiceUnits)
    clock = 1
    Next_customer = 1
    
    while (Customers < N+indkoersel){
      a=a+1
      if(Customers == indkoersel){ # Indkoersel
        Start_customer = Customers
        Start_B = B
      }
      
      if (any(S == clock)){ # er svare clock til at i kunder er serviceret
        S[S==clock] = 0 # vi laver en tom plads
        
      }
      
      if (Next_customer == clock){ # svare event til at der kommer en ny kunde
        Customers = Customers + 1
        Next_customer = clock + X_customer[a] # Tiden for hvorn??r n??ste kunde kommer beregnes
        if (any(S == 0)){ # Hvis der er en tom plads beregnes tiden for hvor langt tid det tager at ekpiderer kunder der tager denne tomme plads
          S[S==0][1] = X_service[a] + clock
        } else { # hvis der ikke er en tom plads bliver kunden blockerer og ender i B
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
  
#   return(print(output))
# }



