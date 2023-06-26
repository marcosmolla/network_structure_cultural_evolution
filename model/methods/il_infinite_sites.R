#
## Code for 'Implications of network structure for cultural evolution'
#

# A function that takes the populatin repertoire and parameters from the setup object. It simulates infinite sites innovation process, i.e. selecting a trait that is absend in the population
setGeneric('il_infinite_sites', function(S, NP, REP, ADJ_N) standardGeneric('il_infinite_sites'))
setMethod(f = 'il_infinite_sites', signature = c(S='data.frame', NP='integer', REP='matrix', ADJ_N='ANY'), definition = function(S, NP, REP, ADJ_N){
 # Learn random float between 0 and 1; Note that R stores 16 decimal places which can be printed using, e.g. sprintf("%.31f", runif(1,0,1))
 return( as.character(dqrunif(n = 1,min = 0,max = 1)) )
})
