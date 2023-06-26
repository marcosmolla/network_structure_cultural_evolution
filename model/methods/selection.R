#
## Code for 'Implications of network structure for cultural evolution'
#

setGeneric('selection_no_death', function(...) standardGeneric('selection_no_death'))
setMethod(f = 'selection_no_death', definition = function(){
 return(NULL) 
})

setGeneric('selection_neutral', function(S) standardGeneric('selection_neutral'))
setMethod(f = 'selection_neutral', signature = c(S='data.frame'), definition = function(S){
 # return( sample.int(n = S$"n", size = 2, replace = F) )
 return( dqsample(x = S$"n", size = 2, replace = F) )
})
