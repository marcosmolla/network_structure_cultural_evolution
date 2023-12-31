#
## Code for 'Implications of network structure for cultural evolution'
#

## Set up Ewens Waterson Neutrality test funuction ----
get_WFpredictions <- function(mysample,SIMMAX){
  
  # Setting Ecount and Fcount to 0
  Ecount <- 0
  Fcount <- 0
  
  # the rest is calculated based on mysample
  k <- length(mysample) #3
  nSam <- sum(mysample) #20
  simpson0 <- sum( (mysample/nSam)^2 )
  pr0 <- 1/prod(mysample)
  
  # loading diversity values generated by simMax configurations possible under neutral evolution, i . e . sampled from Ewens sampling distribution, with nSam and k
  # name <- sprintf('./data/WFprediction/singletons_WF_n%02d_k%02d.mat',nSam,k)
  director <- 'WFpredictions'
  name <- paste(director, sprintf('WF_n%02d_k%02d.RData',nSam,k), sep="/")
  
  # If the file already exists 
  if(file.exists(name)){
    # Sys.sleep(time = 1)
    load(name) # load single
    load(paste(director, sprintf('WF_n%02d_k%02d.RData',nSam,k), sep="/")) # load simpson
    # load(paste(director, sprintf('prob_WF_n%02d_k%02d.RData',nSam,k), sep="/")) # loading probability values generated by simMax configurations possible under neutral evolution with nSam and k
    
    index_s <- simpson <= simpson0 # find all diversity values smaller than the oberved one
    Fcount <- sum(index_s)/length(simpson) # estimate of the probability that the observed sample has a diversity smaller than samples generated under neutral evolution
    index_p <- pr <= pr0 # find all probability values smaller than the oberved one
    Ecount <- sum(index_p)/length(pr)  # estimate of the tail probability that the observed sample has a probability smaller than samples generated under neutral evolution
    
    
  } else {
    B <- matrix(0, ncol=nSam+1, nrow = nSam+1)
    B[1,1] <- 1
    
    for(k1 in 1:nSam){
      for(n1 in 1:nSam){
        B[k1+1,n1+1] <- (k1*B[k1,n1]+(n1-1)*B[k1+1,n1])/(n1)
      }
    }
    
    B <- B[-1,]
    B <- B[,-1]
    
    simMax <- SIMMAX # do 1e6
    single <- rep(0, simMax)
    maxFreq <- rep(0, simMax)
    pr <- rep(0, simMax)
    simpson <- rep(0, simMax)
    
    for(sim in 1:simMax){
      if(k > 1){
        n <- rep(1, k)
        # choosing n_1
        alpha <- runif(1,0,1)
        P <- B[k-1,nSam-n[1]]/(B[k,nSam]*n[1])
        while(P < alpha){
          n[1] <- n[1] + 1
          P <- P+B[k-1,nSam-n[1]]/(B[k,nSam]*n[1])
        }
        # choosing n_i, i=2, ..., k-1
        for(i in 2:(k-1)){ # NOTE, this requires k > 2
          alpha <- runif(1,0,1)
          P <- B[k-i,nSam-sum(n[1:i])]/(B[k-i+1,nSam-sum(n[1:(i-1)])]*n[i])
          while(P < alpha){
            n[i] = n[i]+1
            P <- P + B[k-i,nSam-sum(n[1:i])]/( B[k-i+1,nSam-sum(n[1:(i-1)])]*n[i] )
          }
        }
        n[k] <- nSam - sum(n[1:(k-1)]) # the last one is simply whatever is left
        
        single[sim] <- sum(n==1)
        maxFreq[sim] <- max(n)/nSam
        nRel <- n/nSam
        simpson[sim] <- sum(nRel^2)
        pr[sim] <- 1/prod(n)
      } else {
        simpson[sim] <- 1
        pr[sim] <- 1/nSam
        single[sim] <- 0
        maxFreq[sim] <- 1
      }
    }
    
    # index <- pr<=pr0
    # Ecount <- sum(index) / simMax
    Ecount <- sum(pr<=pr0) / simMax
    # index <- simpson<=simpson0
    # Fcount <- sum(index) / simMax
    Fcount <- sum(simpson<=simpson0) / simMax
    
    save(simpson,pr,single,maxFreq, file=paste(director, sprintf('WF_n%02d_k%02d.RData',nSam,k), sep="/")) 
    # save('pr', file=paste(director, sprintf('prob_WF_n%02d_k%02d.RData',nSam,k), sep="/")) 
    # save('single', file=paste(director, sprintf('singletons_WF_n%02d_k%02d.RData',nSam,k), sep="/")) 
    # save('maxFreq', file=paste(director, sprintf('max_WF_n%02d_k%02d.RData',nSam,k), sep="/")) 
    
  }
  return(matrix(c(Ecount,Fcount,k), nrow=1))
}
