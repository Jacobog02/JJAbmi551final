scale_data <- function(mat){
  sub_data <- mat[,2:ncol(mat)]
  classif <- mat[,1]
  
  maxs <- apply(sub_data,2,max)
  mins <- apply(sub_data,2,min)
  
  
  scaled <- as.data.frame(scale(sub_data, center=mins, scale = maxs-mins))
  
  kmat <- cbind(classif,scaled)
  
  return(kmat)
}


E_recode <- function(mat){
  classif <- ifelse(mat$classif == 'EWS', 1, 0)
  
  mat$classif <- classif
  
  return(mat)
  
}


# Calucates the percent accurace for kfold 
perc_acc <- function(result, truth, o=0){
  
  # Accuracy is 0 at first
  test.ac = 0
  n <- length(truth)
  
  # Operator used in order to expand to additional loops,
  # So the length of the vector is multiplied by  however many loops you do to find the average.
  if (o){
    
    oper <- n * o
    
    
  } else{
    oper <- n
  }
  
  
  for (i in 1:n){
    if (result[i] == truth[i]) {
      
      test.ac <- test.ac + (1/oper) 
    }
    
  }
  return (test.ac)
}
