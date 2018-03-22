source("Rposimv2.R")
#set.seed(12345)


OStack <- initOS(1000, 2, appcols = NULL, app_parameters=c('total_up_time', 'num_breakdowns', 'immediate_rep'), resource=list(num_repair_men = 1))

for(i in 1:2){
  activate(OStack, "App2.R")
}

simulate(OStack)


total_time <- 0
#break_downs <- 0
#imm_rep <- 0

for(i in 2:3){
  
  total_time <- total_time + OStack$thread_stack[i,'total_up_time'] 
  break_downs <- break_downs + OStack$thread_stack[i,'num_breakdowns'] 
  imm_rep <- imm_rep + OStack$thread_stack[i,'immediate_rep'] 
}


cat("Portion of Up_Time: \n")
print (total_time/(2 * OStack$max_time))
#cat("Portion of Immediate Repairs\n")
#print (imm_rep/break_downs)

cat("Time Now\n")
print(OStack$now)
