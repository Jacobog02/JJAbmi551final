infinite_generator <- function(mat , k = 1){
  i <-0
  n <- length(mat)
  
  if (k > n){
   return(NULL) 
  }
  
  # Initialize the Function Here 
  
  function(){
    
    for (k in seq(n)){
      i <<- i + 1
      print(k)
      return(i)
    }
  }
}


list = c(1,2,3,4,5)
gen <- infinite_generator(list) 
gen() #1 
gen() #2 
gen() #3