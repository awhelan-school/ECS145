library(simmer)
setwd('.')
set.seed(12345)


# Simulation Parameters
NUM_MACHINES <- 2
MAX_SIM_TIME <- 10000.0
TOTAL_UP_TIME <- 0

# Initializes Class and Parameters for Application
Machine <- list()
Machine$up_rate <- 1.0/1.0
Machine$rep_rate <-  1.0/0.5
Machine$total_up_time <- 0.0


env <- simmer('Mach_Rep')

# Write your Run Process Method Here
Run <- function(machine){
  
    trajectory() %>%
      set_attribute("total_up_time", 0) %>% 
      # Set Current Time
      set_attribute("start_time", function() now(env)) %>% 
      
      seize(machine, 1) %>%
      
      log_("Simulate Up Time\n") %>%
      timeout(function() rexp(1, Machine$up_rate)) %>%
      # Update Total Up Time
      
      set_attribute('up_time', function() now(env) - get_attribute(env, "start_time")) %>%
      set_attribute('total_up_time', function() get_attribute(env, "up_time"), mod="+") %>%  
    
      log_("Simulate Repair Time\n") %>%
      timeout(function() rexp(1, Machine$rep_rate)) %>%
      release(machine, 1) %>%
      rollback(9,Inf) # Simulate Cycle of Uptime and Repair
    
}



machines <- paste("machine", 1:NUM_MACHINES)

for (i in machines) {
  env %>%
  add_resource(i, 1, Inf) %>%
  add_generator(i, Run(i), at(0), mon = 2)
} 


env %>% run(MAX_SIM_TIME)


x <- tail(get_mon_attributes(env))

TotalUpTime <- sum(x[x[,3] == 'total_up_time',4])

cat("the percentage of up time was :") 
cat(TotalUpTime/(2*MAX_SIM_TIME))


