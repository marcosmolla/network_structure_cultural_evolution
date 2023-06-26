#
## Code for 'Implications of network structure for cultural evolution'
#

# Wrapper function that takes a parameter matrix and (optionally) a folder directory for storing simulation results, and runs all parameter combinations and repetitions
setGeneric('runSNSL', function(PARAMETERS,SIM,IL,SL,REC,DIR,MC=2) standardGeneric('runSNSL'))
setMethod(f = 'runSNSL', signature = c(PARAMETERS="data.frame",SIM="function",IL="function",SL="function",REC="function",DIR="ANY",MC="ANY"), definition = function(PARAMETERS,SIM,IL,SL,REC,DIR,MC){
  if(any((!PARAMETERS$moran) * PARAMETERS$vtrans)) stop("Not defined (Moran: false + Vertical transmission: true")
  if(any((!PARAMETERS$vtrans) * is.infinite(PARAMETERS$m))) stop("Not defined (infinte traits but no vertical transmisison).")
  
  # If no directory is set, run simulation and return (not save in file) results
  if (missing(DIR)) {
   
   # Run simulations w/o multicore
   RES <- lapply(1:nrow(PARAMETERS), function(run){
    SIM(SETUP      = PARAMETERS[run,   ],
        IL         = IL,
        SL         = SL,
        REC        = REC,
        REPETITION = PARAMETERS[run,"r"])
   })
   
  } else {
   
   # Run simulations w/ multicore
   require(parallel)
   # Check whether directory exists
   if(!dir.exists(paths = DIR)){
    message(paste("Creating directory:",DIR,sep=" "))
    dir.create(DIR)}
   # Run simulations
   message("Running multicore process.")
   RES <- mclapply(mc.cleanup=TRUE, mc.cores = MC, 1:nrow(PARAMETERS), function(run){
    dqset.seed(seed = round(runif(1,0,1)*1e7), stream = run)
    SIM(SETUP      = PARAMETERS[run,   ], 
        IL         = IL,
        SL         = SL,
        REC        = REC,
        REPETITION = PARAMETERS[run,"r"])
   })
  }
  
  # Store results in file, if path is provided
  if(!missing(DIR)){
   save(RES,PARAMETERS, file = paste(DIR,"res.zip",sep="/"), compress = "gzip")
   message("Done!")
  }
  
  # Return results
  return(RES)
})
