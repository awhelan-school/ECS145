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

# Application Cols will be event specific parameters
# Application Parameters will be Simulation specific variables to observe
initOS <- function(max_time, num_threads, appcols=NULL, app_parameters=NULL){
  
  # Global Environment
  OStack <- new.env()
  
  # OS varibales
  OStack$now <- 0
  OStack$max_time <- max_time
  OStack$num_threads <- num_threads + 1
  OStack$active_process <- 1
  OStack$thread_id <- 1
  
  # Event List
  OStack$event_list <- matrix(nrow = OStack$num_threads, ncol = 2 + length(appcols))
  colnames(OStack$event_list) <- c('Event Time', 'ThreadID', appcols)
  
  # Thread Stack
  options(bigmemory.allow.dimnames = T)
  OStack$thread_stack <- bigmemory::big.matrix(nrow=OStack$num_threads, ncol = 2 + length(app_parameters), init = 1)
  colnames(OStack$thread_stack) <- c('Active', 'Time', app_parameters)
  
  
  thread_stack <- bigmemory::describe(OStack$thread_stack)
  save(thread_stack, file='thread_stack')
  
  OStack$thread_stack[1, 2] <- OStack$now
  OStack
}

activate <- function(OStack){
  id <- getID(OStack)
  system2(command = "xterm", args = sprintf("-e Rscript App.R \"%s\" &", id ))
  
}

simulate <- function(OStack){
  
  for (i in 2:OStack$num_threads)
  {
    yield(wait = OStack$now, thread_id = 1, res_id= i, ts=OStack$thread_stack)
  }
  
  while(OStack$now < OStack$max_time)
  {
    OStack$thread_stack[1,2] <- OStack$now
    
    #set a wait for all threads to finish?
    
    for (i in 2:OStack$num_threads)
    {
      if (OStack$thread_stack[i, 2] != 0)
      {
        OStack$event_list[i, 1] <- OStack$thread_stack[i, 2] + OStack$now
        OStack$event_list[i, 2] <- OStack$thread_stack[i, 1]
      }
    }
    if(length(OStack$event_list[1,]) != 0)
    {
      cat("Enter Event Process\n")
      #event has occured
      #delete the event
      this_event_thread <- which.min(as.vector(OStack$event_list[,1]))
      cat("Enter Event Process 2\n")
      this_event_time <- OStack$event_list[this_event_thread,1]
      cat("Enter Event Process 3\n")
      OStack$event_list[this_event_thread,] <- NA
      #increment time
      OStack$now <- OStack$now + this_event_time
      cat("Enter Event Process 4\n")
      
      print(OStack$thread_stack)
      print(OStack$event_list)
      print(as.character(this_event_thread+1))
      print(as.character(OStack$now))
      
      OStack$thread_stack[1, 2] <- OStack$now
      print(OStack$now)
      
      #yield to activated event's thread
      print(OStack$now, this_event_thread)
      yield(wait = OStack$now, thread_id = 1, res_id= this_event_thread, ts=OStack$thread_stack)
      cat("Enter Event Process 5")
    }
    cat("Exit Event Process")
  }
  
}


yield <- function(wait, thread_id, res_id, ts){
  
  cat("Start Yield\n")
  ts[thread_id, 2] <- wait
  ts[thread_id, 1] <- 0
  
  ts[res_id, 1] <- 1
  # hold while thread is inactive
  while (ts[thread_id, 1] == 0){}
  cat("Finished Yield\n")
  
}

get_time <- function(ts){
  return(ts[1,2])
}

nextEvent <- function(){
  
  
}

run <- function(){
  
  
}


getID <- function(OStack){
  OStack$thread_id <- OStack$thread_id + 1
}
