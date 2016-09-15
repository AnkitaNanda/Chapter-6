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
  
  print(lambda)
  
  j <- l
  i <- 1
  
  for(j in l:2 ){
    
    br <- readline(paste("Enter the transition rate from  state ", j-1 , "to state ",j-2, ": "))
    mu[i] <- as.double(br)
    j <- j-1
    i <- i+1
  }
  
 print(mu) 
  
 
 sum <- 1
 i <- 1
 p_o <- 1
 
 for(i in 1:length(lambda)){
   
   p_n <- p_o * lambda[i]/mu[i]
   sum <- sum + p_n
   p_o <- p_n
   i <- i+1
 }
 
 pi_zero <- (1/sum)
 
 print(paste("The probability that the system is empty is: " , pi_zero))
 
 
 s <- readline("Enter the state you want to evaluate: ")
 s <- as.integer(s)
 
 print(s)
 
 
 i <- 1
 p_o <- 1
 p_n <- 1
 for(i in 1:s){
   p_n <- p_o * lambda[i]/mu[i]
   p_o <- p_n
   i <- i+1
  
 }
 
 pi_s <- pi_zero * p_n
 
 
 print(paste("The probability that the system is in state ", s, " is: " , pi_s))
 

 
 
}

bd_process()
