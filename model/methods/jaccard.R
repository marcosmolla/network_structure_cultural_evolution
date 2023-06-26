#
## Code for 'Implications of network structure for cultural evolution'
#

# Calculate Jaccard distance of two boolean vectors of the same length
setGeneric('jaccard', function(A,B) standardGeneric('jaccard'))
setMethod(f = 'jaccard', signature = c(A='logical', B='logical'), definition = function(A,B){
 if(length(A)!=length(B)) stop("The two vectors have different length and cannot be compared.")
 if(sum(A*B)==0) 
  return( 0 )
 else 
  return( sum(A*B) / (sum(A)+sum(B)-sum(sum(A*B))) )
})

setMethod(f = 'jaccard', signature = c(A='matrix'), definition = function(A){
 if(!is.logical(A[1])) stop("The matrix does not contain logical/boolean values.")
 res<-matrix(NA,nrow=nrow(A),ncol=nrow(A))
 for(i in 1:(nrow(A)-1)){
  for(j in (i+1):nrow(A)){
   res[i,j]<-jaccard(A = A[i,], B = A[j,])
  }
 }
 return( res )
})

# a<-c(T,T,F,T,F)
# b<-c(F,F,T,T,T)
# jaccard(a,b)
# 
# a<-matrix(sample(c(F,T),120,T), nrow=10,ncol=12)
# jaccard(A = a)
