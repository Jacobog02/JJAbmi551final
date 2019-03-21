scale_data <- function(mat){
  #sub_data <- mat[,2:ncol(mat)]
  #classif <- mat[,1]
  sub_data = mat
  maxs <- apply(sub_data,2,max)
  mins <- apply(sub_data,2,min)
  
  
  scaled <- as.data.frame(scale(sub_data, center=mins, scale = maxs-mins))
  
  #kmat <- cbind(classif,scaled)
  
  return(scaled)
}


E_recode <- function(mat){
  classif <- ifelse(mat$classif == 'EWS', 1, 0)
  
  mat$classif <- classif
  
  return(mat)
  
}


# Calucates the percent accurace for kfold 
perc_acc <- function(result, truth, o=0){
  
  
  rows <- dim(truth)[1]
  cols <- dim(truth)[2]
  
  
  # Operator used in order to expand to additional loops,
  # So the length of the vector is multiplied by  however many loops you do to find the average.
  if (o){
    
    oper <- rows * o
    
    
  } else{
    oper <- rows
  }
  
  accuracy_list = c()
  for (c in seq(cols)){
    
    test.ac <- 0# Accuracy is 0 at first
    for (i in 1:rows){
      if (result[i,c] == truth[i,c]) {
        
        test.ac <- test.ac + (1/oper) 
      }
    }# End of row loop!
    
    # Now append the columps
    accuracy_list <- c(accuracy_list,c(test.ac))  
  }
  
  return (accuracy_list)
}


mat_zip <- function( mat1, mat2, id_list=list()) {
  # Make function make it take 3 arguements ( seed frame (The shared x axis or something), matrix 1, Matrix2 )
  # This makes the two matrixes consecutive 
  # Idenfitier = nns which is the  number of nodes vector. 
  
  n <- length(mat1)
  k <- nrow(mat1)
  
  if (!length(id_list)){
    id_list = list(nodes = seq(n), keys = c('train_key_"','test_key_')) 
  }
  
  # Begin Processing matrixes
  for (i in seq(n)){
    
    buffer <- data.frame(mat1[i],mat2[i])
    nodes_buffer  <- id_list$nodes[[i]]
    
    if (length(nodes_buffer) != 1) {
      
      nodes <- paste(nodes_buffer,collapse='x')
      
    } else{
      
      nodes <- paste(nodes_buffer)
    }
    #print(nodes)
    updated_names <- c(paste(id_list$keys[1],nodes,sep=''),paste(id_list$keys[2],nodes,sep = ''))
    
    colnames(buffer) <- updated_names
    if (i == 1){
      organized_matrix <- buffer
    } 
    else{
      organized_matrix <-  cbind(organized_matrix, buffer) 
    }
  }
  
  #Now clean names
  return(organized_matrix)
}
