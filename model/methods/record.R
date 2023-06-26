#
## Code for 'Implications of network structure for cultural evolution'
#

# Record functions ----

## For single memory slot and two traits ----
### 2 traits, many, large ----
record_2trait_pop_large <- function(S, INDEX, REP, ADJ, REC, ...){
 #Record functions should always return a single value or a vector of values
 if(is.null(INDEX)){
  return(list(trait_pop = rep(NA, S$g),
              prop_in_neib = matrix(data=NA, ncol=S$n, nrow=S$g),
              repertoire = matrix(data=NA, ncol=S$n, nrow=S$g),
              adjm = NULL,
              g=0
  )
  )
 } else {
  if(INDEX %% S$"n" == 0){
   i <- REC$"g" <- REC$"g" + 1
   # Proportion of population with the trait
   REC$"trait_pop"[i] <- sum(REP)/S$n
   # Average proportion of neighbours with trait A (ADJ%*%REP returns the number of neighbours with trait, rowSums returns an individual's degree)
   REC$"prop_in_neib"[i,] <- (ADJ %*% REP) / rowSums(ADJ)
   # A snapshot of the repertoire
   REC$"repertoire"[i,] <- REP
   # Record network in the last round
   if(i == S$"g" & S$"p_r"!=-1 & S$"storenet") REC$adjm <- ADJ
  }
  return(REC)
 }
}

### 2 traits, many (time course) ----
record_2trait <- function(S, INDEX, REP, ADJ, REC, NEW){
 #Record functions should always return a single value or a vector of values
 if(is.null(INDEX)){
  return(list(trait_pop = rep(NA, S$g),
              adjm = NULL,
              snapshot_rep = NULL,
              # when_learned = rep(S$n*S$g, S$n),
              prop_in_neib = matrix(data=NA, ncol=S$n, nrow=1), # only last turn
              g = 0
  )
  )
 } else {
  if(INDEX %% S$"n" == 0){
   i <- REC$"g" <- REC$"g" + 1
   # Proportion of population with the trait
   REC$"trait_pop"[i] <- sum(REP)/S$n
   # if(!is.null(ID)) REC$"when_learned"[NEW] <- i
   # Record network in the last round
   if(i == (S$"g")){
    if(S$"storenet") REC$adjm <- ADJ
    REC$snapshot_rep <- REP
    REC$"prop_in_neib" <- (ADJ %*% REP) / rowSums(ADJ)
    }
  }
  return(REC)
 }
}


## For single memory slot and infinite traits ----
### INF traits, large ----
record_inf_trait_pop_large <- function(S, INDEX, REP, ADJ, REC, ...){
 #Record functions should always return a single value or a vector of values
 if(is.null(INDEX)){
  return(list(c_pop = rep(NA, S$g),
              c_simpson = rep(NA, S$g),
              repertoire = matrix(data=NA, ncol=S$n, nrow=S$g),
              adjm = NULL,
              g = 0
  )
  )
 } else {
  if(INDEX %% S$"n" == 0){
   i <- REC$"g" <- REC$"g" + 1
   # Proportion of population with the trait
   REC$"c_pop"[i] <- length(unique(REP))
   REC$"c_simpson"[i] <- simpson(REP)
   # A snapshot of the repertoire
   REC$"repertoire"[i,] <- REP
   # Record network in the last round
   if(i == (S$"g") & S$"p_r"!=-1 & S$"storenet") REC$adjm <- ADJ
  }
  return(REC)
 }
}

### INF traits, many (for time course) ----
record_inf_trait_pop <- function(S, INDEX, REP, ADJ, REC, ...){
 #Record functions should always return a single value or a vector of values
 if(is.null(INDEX)){
  return(list(c_pop = rep(NA, S$g),
              c_simpson = rep(NA, S$g),
              adjm = NULL,
              g = 0
  )
  )
 } else {
  if(INDEX %% S$"n" == 0){
   i <- REC$"g" <- REC$"g" + 1
   # Number of unique variants at this time step
   REC$"c_pop"[i] <- length(unique(REP))
   REC$"c_simpson"[i] <- simpson(REP)
   # Record network in the last round
   if(i == (S$"g") & S$"p_r"!=-1 & S$"storenet") REC$adjm <- ADJ
  }
  return(REC)
 }
}

### INF traits, many, hash (for EK and ET calculations) ----
# This is a version where I use hashes with a key (trait ID) and a value (rounds present in the population)
record_inf_trait_pop_hash <- function(S, INDEX, REP, REC, ADJ, NEW){
 #Record functions should always return a single value or a vector of values
 if(is.null(INDEX)){
  # For recording values as hashes
  # create hash environment
  hash   <- new.env(hash=TRUE, parent=emptyenv(), size=100L)
  hash_t <- new.env(hash=TRUE, parent=emptyenv(), size=100L)
  unique_initial <- as.character(unique(REP))
  assign_hash(x=unique_initial,value = rep(1, length(unique_initial)),hash)
  assign_hash(x=unique_initial,value = 1, hash_t)
  # For recording values in list
  return(list(EK = rep(0L, S$"g"),#length(unique_initial)
              # Simpson = rep(NA, S$g*S$n),
              ht = hash,
              ht_t = hash_t,
              snapshot_rep = rep("A", S$"n"),
              CPOP = c(1L)#,edgel = NULL
  )
  )
 } else {
  u_pop <- unique.default(as.vector(REP)) # turn REP matrix into c() vector
  
  # E[T],increment counter if trait is present in population 
  repnew <- REP[NEW]
  if( is.null( REC$ht[[repnew]] ) ){
   REC$ht[[repnew]] <- 0
   REC$ht_t[[repnew]] <- INDEX
   REC[["CPOP"]] <- 1L + REC[["CPOP"]] # if a new trait appears, increase cpop counter by 1
  }
  
  for(x in u_pop){
   REC$ht[[x]] <- REC$ht[[x]] + 1L
  } # turns out that the for loop here is faster than using mapply
  
  # E[K] Number of unique variants at this timestep
  if(INDEX %% S$"n" == 0){
   REC[["EK"]][match(0, REC[["EK"]])] <- length(u_pop)} # Here identical to E[K]
  # REC$EK[INDEX] <- length(u_pop) # Here identical to E[K]
  # REC$EK <- sum(REC[["EK"]], length(u_pop))/2 # Here identical to E[K]; just keeping a rolling mean
  
  # REC$"Simpson"[INDEX] <- simpson(REP)
  # Store snapshot of repertoire in the last time step
  if(INDEX == (S$"g"*S$"n")){
   # Take a snapshot of the repertoire of all individauls in the last round
   REC$snapshot_rep <- REP
   # If TRUE, store the network as an edgelist (except for the fully connected graph)
   if(S$"storenet"){
    if(S$"p_r"!=-1){
      REC$edgel <- get.edgelist(graph_from_adjacency_matrix(ADJ))
    } else {
      REC$edgel <- "fully connected"
    }
   }
   # Finally, summarise the two hash tables that kept track of trait innovation time and trait survival time
   id <- ls(REC$ht)
   REC$ET <- data.frame(#trait=as.numeric(id),
              ET=get_hash(id, REC$ht),
              ET_t=get_hash(id, REC$ht_t), stringsAsFactors = F, row.names=NULL)
   REC$ht <- REC$ht_t <- NULL
  }
  return(REC)
 }
}

### INF traits, last round repertoire snapshot only ----
record_inf_trait_lastonly <- function(S, INDEX, REP, REC, ADJ, ...){
 if(is.null(INDEX)) return(list(snapshot_rep = rep("A", S$"n"),
                                EK = rep(0L, S$"g"),
                                lambdaGlobal = 0L,
                                lambdaLocal = rep(0L, S$"n"),
                                edgel = NULL))
 if(INDEX %% S$"n" == 0){
  REC[["EK"]][match(0, REC[["EK"]])] <- length(unique.default(as.vector(REP)))
  if(INDEX == (S$"g"*S$"n")) {
   REC[["snapshot_rep"]] <- REP
   # Calculate global heterogeneity 
   REC[["lambdaGlobal"]] <- simpson(REP)
   # Calculate local heterogeneity (for each individual and its neighbourhood
   # Need to add a self-link to include the focal individual 
   adj_n <- ADJ
   diag(adj_n) <- 1
   REC[["lambdaLocal"]] <- apply(X=adj_n, 2, FUN=function(x) simpson(REP[x==1,]))

   if(S$"storenet"){
    if(S$"p_r"!=-1){
      REC$edgel <- get.edgelist(graph_from_adjacency_matrix(ADJ))
    } else {
      REC$edgel <- "fully connected"
    }
   }
  }
 }
 return(REC)
}
