# Rposim.R: R routines for discrete-event simulation (DES), process oriented paradigm

# OStack 
# Description: This environment will contain application specific parameters supplied by the user.
#              The OS/Manager will be initialized as a global environment and will contain these variables.
#
# Components: Variables used by OS Manager in the simulation 
#             now: Current simulation time
#             max_time: The simulation duration time 
#             num_threads: User defined application thread count.
#             thread_stack: big memory matrix of thread information used to communicate which threads are active/waiting
#
            

# Library Functions:
#
#             initOS(): An initialization for the OS/Manager that sets up all variables
#                       and data structures used in the simulation.
#
#             activate(): This function will start the thread and will execute 
#                         the application specific code specified by the Run method.
#
#             yield(): Starts the busy-while loop and initiates another thread to run. 
#

library(bigmemory)

# Application Cols will be event specific parameters
# Application Parameters will be Simulation specific variables to observe
initOS <- function(max_time, num_threads, appcols=NULL, app_parameters=NULL, resource=NULL){
  
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
  OStack$thread_stack <- bigmemory::big.matrix(nrow=OStack$num_threads, ncol = 3 + length(app_parameters) + length(resource), init = 1)
  
  colnames(OStack$thread_stack) <- c('Active', 'Time', 'Operation', app_parameters, names(resource))
  
  # Set OS Resouce Values
  values <- as.vector(unlist(resource))
  start = (3+length(app_parameters)+1)
  end = start + length(resource) - 1
  OStack$thread_stack[1,start:end] <- values

  
  thread_stack <- bigmemory::describe(OStack$thread_stack)
  save(thread_stack, file='thread_stack')
  
  OStack$thread_stack[1, 2] <- OStack$now
  OStack
}

activate <- function(OStack, fname=NULL){
  id <- getID(OStack)
  system2(command = "xterm", args = sprintf("-e Rscript \"%s\" \"%s\" &", fname, id ))
  
}

simulate <- function(OStack){
  
  while(OStack$now < OStack$max_time)
  {
    OStack$thread_stack[1,2] <- OStack$now
    
    cat("ERROR4")
    
    #set a wait for all threads to finish?
    
    for (i in 2:OStack$num_threads)
    {
      # Thread Has not Run not Run since last iteration
      if (OStack$thread_stack[i, "Time"] != 0)
      {
        OStack$event_list[i, 1] <- OStack$thread_stack[i, 2] + OStack$now
        OStack$event_list[i, 2] <- OStack$thread_stack[i, 1]
        # Reset Thread's Time Parameter
        OStack$thread_stack[i, "Time"] <- 0
      }
    }
    if(length(OStack$event_list[1,]) != 0)
    {
      #event has occured
      #delete the event
      this_event_thread <- which.min(as.vector(OStack$event_list[,1]))
      this_event_time <- OStack$event_list[this_event_thread,1]
      this_event_operation <- OStack$thread_stack[this_event_thread, 'Operation']

      OStack$event_list[this_event_thread,] <- NA
      
      cat("ERROR5\n")
      # Hold
      if(this_event_operation == 2){
        #increment time
        OStack$now <- this_event_time
        OStack$thread_stack[1, 2] <- OStack$now
      }

      cat("ERROR6\n")
      
      print(OStack$now)
      print(this_event_thread)
      print(OStack$thread_stack)
      
      #yield to activated event's thread
      yield(wait = OStack$now, thread_id = 1, res_id= this_event_thread, ts=OStack$thread_stack)
      
      cat("ERROR7\n")
    }
  }
  
}


yield <- function(func='NULL',resource=NULL, wait, thread_id, res_id, ts){

  print(ts)
  print(thread_id)
  
  ts[thread_id, "Operation"] <- getOperation(func)
  
  print(func)
  
  cat("yield resorce name - ERROR1\n")
  print(resource)
  
  # Resource Available
  if(func == 'request' & !is.null(resource)){

    cat("in request \n")
    if(ts[1,resource] > 0){
      ts[1,resource] <- ts[1,resource] - 1
    }
    else{
      # Wait For Resource
      while (ts[1, resource] == 0){}
    }
  }

  if(func == 'release' & !is.null(resource)){
    
    cat("in release \n")
    ts[1,resource] <- ts[1,resource] + 1
  }
  
  cat("yield - ERROR2\n")
  ts[thread_id, "Time"] <- wait
  ts[thread_id, "Active"] <- 0
  
  
  print(thread_id)
  print(res_id)
  
  # Give Control To OS Thread
  ts[res_id, "Active"] <- 1
  
  cat("yield - ERROR3\n")
  # hold while thread is inactive
  while (ts[thread_id, "Active"] == 0){}
  
  cat("yield - ERROR4\n")
  
  
}

now <- function(ts){
  return(ts[1,"Time"])
}


getID <- function(OStack){
  OStack$thread_id <- OStack$thread_id + 1
}

getOperation <- function(op){
  
  if(is.null(op))
    return(1)
  
  defaultOps <- c('NULL', 'hold', 'request', 'release', 'passivate', 'cancel')
  
  return(which(defaultOps == op))
  
}
