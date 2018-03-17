source('Rposim.R')



OStack <- initOS(10000, 2, appcols = NULL, app_parameters=c('application_parameter'))

for(i in 1:2){
  activate(OStack)
}

simulate(OStack)


total_time <- 0

for(i in 2:3){
  
  total_time <- total_time + OStack$thread_stack[i,3] 

}

print (total_time/(2*OStack$max_time))
