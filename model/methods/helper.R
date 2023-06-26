#
## Code for 'Implications of network structure for cultural evolution'
#

set_repertoire <- function(S){
 if(S$"l"==1){
  if(S$"m"==2){
   return( matrix(data = c(T,rep(x = F,S$n-1)), # begin with all "a" and one "A"
                  ncol=1, 
                  nrow = S$"n") )
  } else if(is.infinite(S$"m")){
   # return( matrix(data = runif(n = S$n, min = 0, max = 1), # begin with random values
   # return( matrix(data = as.character(rep(dqrunif(n = 1, min = 0, max = 1), S$"n")), # begin with one trait
   return( matrix(data = as.character(rep(.555, S$"n")), # begin with one trait
                  ncol=1, 
                  nrow = S$"n") )
  } else {
   stop("No function defined for M != {2, Inf}.")
 }} else {
  stop("No functions defined for memory (l) > 1. ")
 }
}

init_adjm <- function(S){
 if(S$"p_r"==-1){
  # FULLY CONNECTED
  adjm <- matrix(1L, ncol=S$"n", nrow=S$"n")
  diag(adjm) <- 0L
 } else {
  # SMALL WORLD
  adjm <- as.matrix(get.adjacency(small_net(length = S$"n", p = S$"p_r", nei = S$"nei")))
 }
  return(adjm)
}

simpson <- function(REP){
 n_i<-table(REP) # Number of individuals in the i-th species
 n<-length(REP) # Total number of individuals in the community
 sum((n_i*(n_i-1))) / (n*(n-1))
}

# vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

# Calculate the average expected time of absorption/loss/fixation in populations with two traits and innovation diffusion (only transition is from a to A till proportion of A is 1); this depends on population size and innovation rate (social learning efficacy here is assumed to be =1)
t_bar <- function(N,mu){
 prop <- function(N,mu,i) return( ((N-i)/N) *( mu + (1-mu) * (i/N) ) )
 p <- rep(NA, N-1)
 for(i in 1:(N-1)){ p[i] <- prop(N = N, mu = mu, i = i) }
 # plot(p, pch=20)
 return( sum(1/p) )
}
t_bar_v<-Vectorize(t_bar, vectorize.args = c("N","mu"))

# The following two functions are used to calculate the expected number of traits present in a population with infinitely many traits and Moran birth-death replacement (EK) and the expected number of time steps at which a trait is lost from a population (ET)
E_K <- function(N,U){
 theta <- function(N,U) return( (N*U) / (1-U) )
 THETA <- theta(N = N, U = U)
 sum( sapply(0:(N-1), function(i) return(THETA / (THETA+i)) ) )
}
E_T <- function(N,U){ return(E_K(N=N,U=U) / U) }


# A function that shifts IDs such that 1 2 3 4 5 becomes 4 5 1 2 3, and so the individual with the trait is now in the center
shiftID <- function(ID){
 l <- length(ID)
 ID[1:ceiling(l/2)] <- ID[1:ceiling(l/2)]+floor(l/2)
 ID[(ceiling(l/2)+1):l] <- ID[(ceiling(l/2)+1):l]-ceiling(l/2)
 return(ID)
}

# Generates a letter, number, letter combination
setGeneric('identifier', function(...) standardGeneric('identifier'))
setMethod(f = 'identifier', definition = function(...){
 return(paste(c(sample(LETTERS,3,TRUE),sample(0:9,3,T),sample(LETTERS,3,TRUE)),collapse="",sep=""))
})

# A wrapper for the sample function that returns an NA for length 0 and the same value for length 1
setGeneric('mySample', function(n, size=length(n), replace=F, prob=NULL) standardGeneric('mySample'))
setMethod(f = 'mySample', signature = c(n='ANY', size='ANY', replace='ANY', prob='ANY'), definition = function(n, size=length(n), replace=F, prob=NULL){
	if(length(n)>1)  res <- sample(x = n, size = size, replace = replace, prob=prob)
	if(length(n)==1) res <- rep(n, times=size)
	if(length(n)==0) res <- NA
	return(res)
})
