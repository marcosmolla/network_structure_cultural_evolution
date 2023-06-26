#
## Code for 'Implications of network structure for cultural evolution'
#

# This is the Birth/Death version
# IL and SL of a single trait
setGeneric('learn_single_trait', function(NP, ADJ, REP, S, IL, SL) standardGeneric('learn_single_trait'))
setMethod(f = 'learn_single_trait', signature = c(NP="numeric", ADJ="matrix", REP="matrix", S="data.frame", IL="function", SL="function"), definition = function(NP, ADJ, REP, S, IL, SL){
 newborn <- NP[1]
 # parent <- NP[2]
 # Add a self link; this will allow the newborn to look at the individual selected to die
 adj_n<-ADJ[newborn,]
 adj_n[newborn] <- 1L 
 # With probability \mu: Innovate
 if(dqrunif(n = 1,min = 0,max = 1) <= S$"mu"){
  REP[newborn] <- IL(S = S, REP = REP, NP = NP, ADJ_N = adj_n)
  return(REP)
 }
 if(dqrunif(n = 1,min = 0,max = 1) <= S$"beta"){
  # With probability (1-mu): Social learning AND IF a random uniform value is smaller than beta (SL success, or stubbornness parameter)
  REP[newborn] <- SL(S = S, REP = REP, NP = NP, ADJ = adj_n)
  return(REP)
 } 
 # If individual neither innovated nor learned socially
 # newborn acquires base trait 'a' in moran case w/o vertical transmission 
 if(S$moran & !S$vtrans) REP[newborn] <- F
 # newborn acquires trait from parent with certainty in moran case with vertical transmission 
 if(S$moran &  S$vtrans) REP[newborn] <- REP[NP[2]]
 # if none of that is the case, the individual's memory remains unchanged
 return(REP)
})


# # This is a Death/Birth version
# # IL and SL of a single trait
# setGeneric('learn_single_trait', function(NP, ADJ, REP, S) standardGeneric('learn_single_trait'))
# setMethod(f = 'learn_single_trait', signature = c(NP="numeric", ADJ="matrix", REP="matrix", S="data.frame"), definition = function(NP, ADJ, REP, S){
#  newborn <- NP[1]
#  parent <- NP[2]
#  # newborn acquires trait from parent with certainty
#  REP[newborn] <- REP[parent]
#  # With probability \mu: Innovate
#  if(runif(n = 1,min = 0,max = 1) < S$mu){
#   REP[newborn] <- individualLearning_memory1(S = S, P = parent, REP = REP)
#  } else if(runif(n = 1,min = 0,max = 1) < S$beta){
#   # With probability (1-mu): Social learning AND IF a random uniform value is smaller than beta (SL success, or stubbornness parameter)
#   REP[newborn] <- socialLearning(S = S, NEW = newborn, REP = REP, ADJ = ADJ)
#  }
#  return(REP)
# })
