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
        Next_customer = clock +  customerDistFunction (customerDist, meanCustomerTime) # Tiden for hvorn??r n??ste kunde kommer beregnes
        if (any(S == 0)){ # Hvis der er en tom plads beregnes tiden for hvor langt tid det tager at ekpiderer kunder der tager denne tomme plads
          S[S==0][1] =  serviceDistFunction(serviceDist, meanServiceTime) + clock
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
  
  return(print(output))
}



