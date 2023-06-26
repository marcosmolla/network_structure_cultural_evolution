#
## Code for 'Implications of network structure for cultural evolution'
#

# Random Oblique Social Learning
setGeneric('sl_random_oblique', function(NP, ADJ, REP, S) standardGeneric('sl_random_oblique'))
setMethod(f = 'sl_random_oblique', signature = c(NP="numeric", ADJ="numeric", REP="matrix", S="data.frame"), definition = function(NP, ADJ, REP, S){
    # IF there are any neighbours 
    # if(any(ADJ[NEW]>0)){
        # select one random demonstrator in neighbourhood
        if(S$"p_r"== -1)
         d <- dqsample(x = S$'n', size = 1) 
        else
         d <- dqsample(x = (1:S$"n")[as.logical(ADJ)], size = 1)
        # Return their one current trait
        return( REP[d] )
    # }
})

