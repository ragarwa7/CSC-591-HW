require(caret)
require(R.matlab)
require(splitstackshape)
require(e1071)

#Q3 part2
SLR <- function(path='hw3q3(b).csv'){
    data <- read.csv(path)

    model <- train(
  		Y ~ X1 + X2 + X3, data,
 	      method = "lm",
            trControl = trainControl(
            method = "cv", number = 10,
            verboseIter = TRUE
            )
	)
    
    print(model)
     
    model2 <- train(
  		Y ~ X1 + X2, data,
 	      method = "lm",
            trControl = trainControl(
            method = "cv", number = 10,
            verboseIter = TRUE
            )
	)
    
    print(model2)


    lm.fit = lm(Y ~ X1 + X2 + X3,data=data)
    lm.fit.coef <- lm.fit$coefficients
    print(lm.fit.coef)

    lm.fit.2 = lm(Y ~ X1 + X2,data=data)
    print(lm.fit.2$coefficients)
}


SVM <- function(path = "hw3q6_cancer.mat"){
   	path <- system.file("mat-files", package="R.matlab")
	pathname <- file.path(path, "hw3q6_cancer.mat")
	data <- readMat(pathname)
	print(length(data$Y[data$Y == 1]))
	print(length(data$Y[data$Y == 0]))


	set.seed(1)
	data_part <- createDataPartition(y = data$Y, 
                                 p = 0.7, list = F)
	test <- data$Y[-data_part,] # 30% data goes here
	train <- data$Y[data_part,] # 70% here
	
	print("________train_________")
      print(length(train[train == 1]))
      print(length(train[train == 0]))

	print("________test_________")
      print(length(test[test == 1]))
      print(length(test[test == 0]))
	
	svfit=svm(data$Y~.,data=data,kernel="radial",gama=1,cost=1)
}



