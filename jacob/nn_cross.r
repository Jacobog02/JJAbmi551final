set.seed(2019) #set the seed
n <- names(ktrain) #extract names to build formula
f <- as.formula(paste('classif ~', paste(n[!n %in% 'classif'], collapse = ' + ')))


k_indexs <- sample(seq(nrow(ktrain))) #scamble the indexs in order to conduct k-fold
nss <- c(1,2,3,4,5,10,20,40,45) # range of hidden layer nodes to explore
ls <- c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1) # range of learning rates to explore
ls_len <- length(ls) # length of the learning rate, i made it same as nodes
zero_ls <- rep(0,ls_len) # empty zero vector to make R happy
train.acc.mat <- data.frame(zero_ls) #initalize to empty vector to make the raw matrix
test.acc.mat <- data.frame(zero_ls) # ^^^


# Node amount loop (outter most loop)
for (s in seq(length(nss))){
  
  ns <- nss[s] #extract node number
  
  
  train.acc <- zero_ls # inilize the accuracy vectors
  test.acc <- zero_ls  # ^^^
  
  # Learning Rate Loop (middle loop)
  for (l in seq(ls_len)){
    lss <- ls[l] #extract the learning rate
    
    # Cross validation loop
    for (i in seq(11)) {
      
      if (i == 1){
        str <- 1 # initalize start index
        stp <- 4 # initialize stop index
      }
      else{
        #4 fold cross validation I chose this way bc the math was easier
        str <- str + 4 
        stp <- stp + 4
      }
      
      k_in <- k_indexs[str:stp] #Pull out the indexs from the shuffled vector
      
      # Subsetting the trianing data 
      fold_cross <- ktrain[k_in,] # Cross validation test set
      fold_c_truth <- ktrain[k_in,1] # Cross validation test truth
      fold_train <- ktrain[-k_in,] # Cross-V training data
      fold_t_truth <- ktrain[-k_in,1] # Cross-V training truth
      
      #Note: The ns can be made into multiple layers by c(layer1,layer2)
      
      
      # Do the THING!               
      nn <- neuralnet(f,data=fold_train, hidden=ns, learningrate = lss, linear.output = F)
      
      
      # compute train predictions
      train.pr <- neuralnet::compute(nn,fold_train[,2:ncol(fold_train)])
      train_result <- round(train.pr$net.result)
      
      # find train accuracy and append to vector
      train.a <- perc_acc(train_result, fold_t_truth,11) #11 cross validation loop ave
      train.acc[l] <- train.acc[l] + train.a
      
      #compute test preditctions
      test.pr <- neuralnet::compute(nn,fold_cross[,2:ncol(fold_cross)])
      test_result <- round(test.pr$net.result)
      
      #find test accuracy and append to vector
      test.a <- perc_acc(test_result,fold_c_truth,11) #11 cross validation loop ave
      test.acc[l] <- test.acc[l] + test.a
      
      
    }
  }
  # add the accuracy vectors to their matrixes
  train.acc.mat <- cbind(train.acc.mat,train.acc) 
  test.acc.mat <- cbind(test.acc.mat,test.acc)
  
}
train.acc.mat <- train.acc.mat[,2:ncol(train.acc.mat)]
test.acc.mat <- test.acc.mat[,2:ncol(test.acc.mat)]