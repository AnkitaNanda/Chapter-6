bd_process <- function(){
  
  l <- readline("Enter the number of states of the system: ")
  print(l)
  l <- as.integer(l)
  lambda <- c(rep(NA,l-1))
  mu <- c(rep(NA,l-1))
  
  i <- 1
  
  for(i in 1:(l-1)){
   
    prompt <- readline(paste("Enter the transition rate from  state ", i-1 , "to state ",i, ": "))
    lambda[i] <- as.double(prompt)
    i <- i+1
    
  }
  
  i <- l
  
  for(i in l:2 ){
    br <- readline(paste("Enter the transition rate from  state ", i-1 , "to state ",i-2, ": "))
    mu[i] <- as.double(br)
    i <- i-1
    
  }
  
  G <- matrix(data = 0, nrow=l, ncol=l) 
  
  i <- 1
  for(i in 1:l){
    
    j <- 1
    
    for(j in 1:l){
      
      if(j==i+1){
        G[i,j] <- lambda[i]
      } 
      
      if(j == i-1) { 
        G[i,j] <- mu[i]
      }
      
      j <- j+1 
      
    }
    
    i <- i+1 
    
  }
  
 Q <- matrix(data = 0, nrow=l, ncol=l) 
 
 i <- 1
 for(i in 1:l){
   
   j <- 1
   
   for(j in 1:l){
     
     if(j==i){
       Q[i,j] <- -sum(G[i,])
     } 
     
     if(j != i) { 
       Q[i,j] <- G[i,j]
     }
     
     j <- j+1 
     
   }
   
   i <- i+1 
   
 }
 
 
print("The generator matrix is given as: ") 
print(Q)
  
}

bd_process()
