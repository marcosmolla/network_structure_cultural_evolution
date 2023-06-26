#
## Code for 'Implications of network structure for cultural evolution'
#

# Random Oblique Social Learning in a innovation diffusion context
setGeneric('sl_random_oblique_diffusion', function(NP, ADJ, REP, S) standardGeneric('sl_random_oblique_diffusion'))
setMethod(f = 'sl_random_oblique_diffusion', signature = c(NP="numeric", ADJ="numeric", REP="matrix", S="data.frame"), definition = function(NP, ADJ, REP, S){
  # select one random demonstrator in neighbourhood
  d <- sample.int(n = S$"n", size = 1, prob = ADJ)
  # Return T if observed trait is T
  if(REP[d]){
   return(TRUE)
  }
  # If observed trait was not true, return parent trait (vertical learning)
  if(S$moran & S$vtrans) return(REP[NP[2]])
  # Or keep the repertoire in the case that this is continuous learning (no vertical transmission)
  return(REP[NP[1]])
})
