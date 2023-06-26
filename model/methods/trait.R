#
## Code for 'Implications of network structure for cultural evolution'
#

# Returns the proportion of a single trait in the population
setGeneric('trait_prop_single', function(REP, N) standardGeneric('trait_prop_single'))
setMethod(f = 'trait_prop_single', signature = c(REP="matrix", N="numeric"), definition = function(REP,N){
 return(sum(REP)/N)
})
