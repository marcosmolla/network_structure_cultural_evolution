# using Statistics, Distributed, Graphs, RCall
# addprocs(100);
# @everywhere using SharedArrays, Graphs

mygrid = collect(Base.product(
    500,                            # N, Number of individuals
    10,                             # K, Degree
    [-1 1 1e-1 1e-2 1e-3 1e-4 0],   # Beta, rewiring
    25e4                             # S, number of simulations
));

for o in 1:length(mygrid)
    # Serialised parameters
    N = Int(mygrid[o][1]);
    K = Int(mygrid[o][2]);
    BETA = mygrid[o][3];
    S = Int(mygrid[o][4]);
    neib = Dict();

    # Create simulation results structure for parallel computing
    global simRes = SharedArray(zeros(Int, S, 2));

    # Run parallel computation for all S simulations of the current parameter set in mygrid
    @sync @distributed for s in 1:S
        # Create network using Julia's Graphs pacakge
        if BETA != -1 # '-1' used to indicate fully connected graph
            generate_network = true;
            while generate_network
                global g = watts_strogatz(N,K,BETA, is_directed=false);
                generate_network = !is_connected(g);
            end
            m = adjacency_matrix(g);
            for k in 1: N 
                m[k,k] = 1;
            end
            
            for l in 1:N
                neib[l] = get(neib, l, findall(m[l,:].==1));
            end
        end

        # Create a repertoire with all 'a' (==false)
        r = fill(0, N);
        # Add a single 'A' variant (== true) randomly
        r[rand(1:N)] = true;
        
        # Set up while loop
        endStatement = true; 
        t = 1; # time step counter
    
        while endStatement
            # sample random individual for replacement
            i = rand(1:N);
            # copy randomly
            if BETA != -1
                r[i] = rand(r[neib[i]]);
            else
                r[i] = rand(r);
            end

            # record frequency 
            freq = sum(r);
    
            if freq==N
                global res = (t, 1);
                endStatement = false
            elseif freq==0
                global res = (t, 0);
                endStatement = false
            else
                t = t+1
            end
        end
        simRes[s,:] .= res; 
    end

    if o==1
        global res = hcat(simRes,fill(N,S), fill(K, S), fill(BETA, S), 1:S);
    else    
        global res = vcat(res, hcat(simRes, fill(N,S), fill(K, S), fill(BETA, S), 1:S));
    end
end

@rput res;
R"""
data <- as.data.frame(res)
colnames(data) <- c('t','fixation','N','K','p_r','r')
setwd("FOLDER_DIRECTORY") # User input
saveRDS(data,'data_simple_sim')
"""
