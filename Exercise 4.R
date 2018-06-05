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

CustomerSimulation = function(traffic,ServiceUnits){
  # Generate U
  N.iter = 100000
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
  lambda = 1
  X_customer = -log(U)/lambda
  X_customer = X_customer[is.finite(X_customer)]
  X_customer = X_customer[!is.na(X_customer)]
  
  lambda = traffic
  X_service = -log(U)/lambda
  X_service = X_service[is.finite(X_service)]
  X_service = X_service[!is.na(X_service)]
  
  ###
  
  N = 10000
  a = 0
  RejectionRate = vector()
  for (i in 1:10){
    B = 0
    Customers = 0
    S = vector('numeric',ServiceUnits)
    clock = 1
    Next_customer = 1
    
    while (Customers < N){
      a = a+1
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
      
      S[S== Inf] = 100000000000
      S[S== NA] = 0
      if (min(S[S!=0])< Next_customer){ # Vi s??tter clock til n??ste gang der kommer en event.
        clock =  min(S[S!=0])
      } else{
        clock =  Next_customer
      }
      
    }
    RejectionRate[i] = B/N
  }
  
  return(print(mean(RejectionRate)))
}
