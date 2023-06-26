#
## Code for 'Implications of network structure for cultural evolution'
#

# Simple individual learning in a two trait scenario, i.e. when successful, switch to opposite

setGeneric('il_two_trait', function(S, REP, NP, ADJ_N) standardGeneric('il_two_trait'))
# setMethod(f = 'il_two_trait', signature = c(S='data.frame', REP='matrix', NP='numeric'), definition = function(S, REP, NP){
#   return(!REP[NP[1]])
# })
setMethod(f = 'il_two_trait', signature = c(S='data.frame', REP='matrix', NP='numeric', ADJ_N='numeric'), definition = function(S, REP, NP, ADJ_N){
 return(!mySample(n = REP[ADJ_N], size = 1)) # do the opposite of a randomly observed neighbour, equivalent to: learn from a neighbour and then innovate, which gets you to the opposite trait
})
