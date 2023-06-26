#
## Code for 'Implications of network structure for cultural evolution'
#

setGeneric('sl_freqdep', function(NP, ADJ, REP, SETUP) standardGeneric('sl_freqdep'))
setMethod(f = 'sl_freqdep', signature = c(NP="integer", ADJ="numeric", REP="matrix", SETUP="data.frame"), definition = function(NP, ADJ, REP, SETUP){
 # d <- NULL
 # if there are any neighbours
 if(any(ADJ>0)){
  # neighbours
  nei <- ADJ!=0
  # record traits and their frequency
  known <- table(REP[nei])
  
  # if there is only one trait known ...
  if(length(known)==1){ 
   if(is.infinite(SETUP$"m")){ # ... in a simulation with inf many traits
    return(names(known)) 
   } else {
    return( REP[nei][1] ) # ... in a simulation with 2 traits
   }
  } else {
   # if there is more than one trait known ...
    weighted_frequency <- known^(1+SETUP$d)
    if(is.infinite(SETUP$"m")){
     items <- names(known)
    } else {
     items <- c(F,T)
    }
    # sample based on weighted frequency
     return( sample(x = items, size = 1, prob = weighted_frequency / sum(weighted_frequency)) )
  }
 }
})

