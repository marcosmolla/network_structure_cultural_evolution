#
## Code for 'Implications of network structure for cultural evolution'
#

# Success biased social learning
setGeneric('sl_success_bias', function(NEW, ADJ, REP, SETUP) standardGeneric('sl_success_bias'))
# In this version, the individual does not learn (nei=0), learns from one neighbour (nei=1), or learns from the best of two random neighbours (nei>1)
setMethod(f = 'sl_success_bias', signature = c(NEW="numeric", ADJ="matrix", REP="matrix", SETUP="data.frame"), definition = function(NEW, ADJ, REP, SETUP){
 # if there are any neighbours
 if(any(ADJ[NEW,]>0)){
  # neighbours
  nei <- which(ADJ[NEW,]!=0)

  # 1 neighbour
  if(sum(nei)==1){
   demonstrator <- nei
  } else {
   # 2 or more neighbours
   if(sum(nei)>1){
    nei <- sample(nei, size = 2, replace = F)
   }
   
   nei_rep <- rowSums(REP[nei,])
   
   if(nei_rep[1]==nei_rep[2]){
    demonstrator <- sample(x = nei, size = 1)
   } else {
    demonstrator <- nei[nei_rep==max(nei_rep)]
   }
  }

  # acquire each of their TRUE traits with probability p_s
  REP[NEW,][REP[demonstrator,]] <- runif(n = sum(REP[demonstrator,]),min = 0,max = 1) <= SETUP$p_s
  stop("Learning incorrectly assigns F to already acquired traits if learning fails. Corrected version is in random_oblique. Implement here!")
 }
 return(REP)
})

