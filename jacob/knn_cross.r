maxK <- 15 # Parameter to optimize

test.acc = rep(0,maxK)
train.acc = rep(0,maxK)

for (k in 1:maxK) {
  for (i in 1:40){
    k_cross_test <- class_mat[i,1:100]
    k_c_class <- class_mat[i,101]
    k_train <- class_mat[-i,1:100]
    cl_class <- class_mat[-i,101] 
    
    
    test_p <-  knn(k_train,k_cross_test,cl_class,k)
    train_p <- knn(k_train,k_train,cl_class,k)
    
    if (test_p == as.factor(k_c_class))
    { 
      #print('test win')
      test.acc[k] <- test.acc[k] + (1/40)
    }
    
    for (p in 1:39)
    {
      
      if (train_p[p] == as.factor(cl_class[p])){
        #print('train win')
        train.acc[k] <-  train.acc[k] + (1/(39*40)) 
      }
    }
    
  }
}
