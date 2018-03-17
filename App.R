source("Rposim.R")
setwd('.')

# Load Thread ID
args = commandArgs(trailingOnly=T)
id <- as.numeric(args[1])

# Loads Shared Memory
load('thread_stack')
tmp <- bigmemory::attach.big.matrix(thread_stack)
assign('ts', tmp)

set.seed(12345)

# Initializes Class and Parameters for Application
Machine <- list()
Machine$up_rate <- 1.0/1.0
Machine$rep_rate <-  1.0/0.5
Machine$total_up_time <- 0.0



# Write your Run Process Method Here
Run <- function(){
  
  while(1){
    
    # Set Current Time
    cTime <- get_time(ts)
    # Hold for exponentially distributed time
    up_time <- rexp(1, Machine$up_rate)
    # Simulate UpTime
    yield(wait = up_time, thread_id = id, res_id = 1, ts = ts)
    # Update Total Up Time
    Machine$total_up_time <- Machine$total_up_time + get_time(ts) - cTime
    # Set Variable in Shared Memory
    ts[id, 'application_parameter'] <- Machine$total_up_time
    # Simulate Repair
    rep_time <- rexp(1, Machine$rep_rate)
    yield(wait = rep_time, thread_id = id, res_id = 1, ts = ts)
    
  }  

}

cat("Starting Thread ")
print(as.numeric(id))
Run()
