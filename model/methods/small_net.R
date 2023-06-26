#
## Code for 'Implications of network structure for cultural evolution'
#

##
## A function to go from lattice over small-world to random
##
small_net <- function(length, p, nei){
 connected <- FALSE
 i<-0
 while(!connected){
  i<-i+1
  if(i==1000) warning("small_net() is having trouble to generate a connected network. Consider increasing 'nei' or reducing 'p'.")
  # start with lattice
  net <- make_lattice(length = length, dim = 1, nei=nei, circular = T)
  # rewire with probability p; for p = 0 (lattice), p = 1 (random), 0 < p < 1 (small-world)
  if(p<0) {
   # warning("p is negative. Setting p = 0")
   p<-0
   }
  net_rew <- rewire(graph = net, with = each_edge(prob = p, loops = FALSE, multiple = FALSE))
  connected <- is_connected(net_rew)
 }
 return(net_rew)
}
