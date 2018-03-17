# Rposim.R: R routines for discrete-event simulation (DES), process oriented paradigm

# OStack 
# Description: This environment will contain application specific parameters supplied by the user.
#              The OS/Manager will be initialized as a global environment and will contain these variables.
#
# Components: Variables used by OS Manager in the simulation 
#             now: Current simulation time
#             max_time: The simulation duration time 
#             active_process: The currently active thread            
#             num_threads: User defined application thread count.
#             thread_stack: big memory matrix of thread information used to communicate which threads are active/waiting
#
#             


# Library Functions:
#
#             initOS(): An initialization for the OS/Manager that sets up all variables
#                       and data structures used in the simulation.
#
#             activate(): This function will start the thread and will execute 
#                         the application specific code specified by the Run method.
#
#             nextEvent(): Will find the minumum time from event list, increment the system
#                          time according to the next event time. Yield to the thread and
#                          set the active process.
#             
#             run(): Will call the next event untill there is no events left or 
#                    max_simulation time is reached. 
#
#             yield(): Starts the busy-while loop and initiates another thread to run. 
#

library(bigmemory)

initOS <- function(max_time, num_threads, appcols=NULL){
  
  # Global Environment
  OStack <- new.env()
  
  # OS varibales
  OStack$now <- 0
  OStack$max_time <- max_time
  OStack$num_threads <- num_threads
  OStack$active_process <- 1
  OStack$thread_id <- 1
  
  # Event List
  OStack$event_list <- matrix(nrow = num_threads, ncol = 2 + length(appcols))
  colnames(OStack$event_list) <- c('Event Time', 'ThreadID', appcols)
  
  # Thread Stack
  options(bigmemory.allow.dimnames = T)
  OStack$thread_stack <- bigmemory::big.matrix(nrow=num_threads, ncol = 2, init = 1)
  colnames(OStack$thread_stack) <- c('Active', 'Time')
  
  
  thread_stack <- bigmemory::describe(OStack$thread_stack)
  save(thread_stack, file='thread_stack')
  
  OStack
}

activate <- function(OStack){
  
  id <- getID(OStack)
  
  system2(command = "xterm", args = sprintf("-e Rscript start.R \"%s\" &", id ))
  
  
}

nextEvent <- function(){
  
  
}

run <- function(){
  
  
}


yield <- function(){
  
  
  
}

getID <- function(OStack){
  
  OStack$thread_id <- OStack$thread_id + 1
  
  
}



OStack <- initOS(100, 5)
activate(OStack)
activate(OStack)

