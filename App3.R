source("Rposimv2.R")
setwd('.')


# Load Thread ID
args = commandArgs(trailingOnly=T)
id <- as.numeric(args[1])

set.seed(id)

# Loads Shared Memory
load('thread_stack')
tmp <- bigmemory::attach.big.matrix(thread_stack)
assign('ts', tmp)

# Initializes Class and Parameters for Application
Machine <- list()
Machine$up_rate <- 1.0/1.0
Machine$rep_rate <-  1.0/0.5
Machine$total_up_time <- 0.0
Machine$Nrep <- 0
Machine$NImmedRep <- 0



# Write your Run Process Method Here
Run <- function(){
  
  while(1){
    
    # Set Current Time
    start_up_time <- now(ts)
    
    # Hold for exponentially distributed time
    up_time <- rexp(1, Machine$up_rate)
    
    # Simulate UpTime
    yield(func = 'hold', wait = up_time, thread_id = id, res_id = 1, ts = ts)
    
    # Update Total Up Time
    Machine$total_up_time <- Machine$total_up_time + now(ts) - start_up_time
    
    # Set Variable in Shared Memory
    ts[id, 'total_up_time'] <- Machine$total_up_time
    
    # Machines Up 
    ts[1,'num_up'] <- (ts[1,'num_up'] - 1)
    
    if(ts[1,'num_up'] == 1){
      
      # Passivate: Wait for Other Machine to go down
      yield('passivate', thread_id = id, res_id = 1, ts = ts)
    
    } else if(ts[1, 'num_repair_men'] == 1){
      
      # Check if Resource is avaiable
      # Reactivate
      # Check if Resource is avaiable
      if(thread_id == 2){
        reactivate_id <- 3
      }
      if(thread_id == 3){
        reactivate_id <- 2
      }
      reactivate(ts, reactivate_id)
      
    }
    
    
    yield(func = 'request', resource = 'num_repair_men', thread_id = id, res_id = 1, ts = ts)
    
    
    # Simulate Repair
    rep_time <- rexp(1, Machine$rep_rate)
    yield(func = 'hold', wait = rep_time, thread_id = id, res_id = 1, ts = ts)
    
    # Machines Up 
    ts[1,'num_up'] <- (ts[1,'num_up'] + 1)
    
    yield(func = 'release', resource = 'num_repair_men', thread_id = id, res_id = 1, ts = ts)
    
  }  
  
}

cat("Starting Thread ")
print(as.numeric(id))
Run()
