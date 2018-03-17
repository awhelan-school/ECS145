file.create('newFile')
args = commandArgs(trailingOnly=T)

print('Hello Darkness') 
print("you are thread ")
print(args[1])


id <- as.numeric(args[1])


load('thread_stack')
tmp <- bigmemory::attach.big.matrix(thread_stack)
assign('x', tmp)

signal <- x[id,1]

while(signal){
  
  signal <- x[id,1]
}

cat("my old friend")







# Write your Run Process Method Here
Run <- function(){
  
  
  
  
}