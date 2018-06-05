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


N = 10000 # Number og customers

RejectionRate = vector()
for (i in 1:10){
  B = 0
  Customers = 0
  S = vector('numeric',8)
  clock = 1
  Next_customer = 1

  while (Customers < N){
    Customers = Customers + 1
    if (any(S == clock)){ # er svare clock til at i kunder er serviceret 
      S[S==clock] = 0 # vi laver en tom plads
      
    }
    
    if (Next_customer == clock){ # svare event til at der kommer en ny kunde
      Next_customer = clock + rexp(1,1) # Tiden for hvorn??r n??ste kunde kommer beregnes
      if (any(S == 0)){ # Hvis der er en tom plads beregnes tiden for hvor langt tid det tager at ekpiderer kunder der tager denne tomme plads
        S[S==0][1] = rexp(1,1/8) + clock
      } else { # hvis der ikke er en tom plads bliver kunden blockerer og ender i B
        B = B+1 
      }
    }
  
    if (min(S[S!=0])< Next_customer){ # Vi s??tter clock til n??ste gang der kommer en event.
      clock =  min(S[S!=0])
    } else{
      clock =  Next_customer
    }
    
  }
  RejectionRate[i] = B/N
}

