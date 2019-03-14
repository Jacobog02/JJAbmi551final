library(randomForest)
model <- randomForest(drug ~ ., data=roots, proximity=TRUE)
return(model)