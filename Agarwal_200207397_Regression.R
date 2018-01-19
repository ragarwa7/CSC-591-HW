require(data.table)
require(caret)
require(InformationValue)
require(bigmemory)

#Q1 
# return: float of the cumulative proportion on tails
CoinFlip <- function(){
	set.seed(1212)
      trials <- 10000
	sampl <- sample(1:0, trials, repl=T)
	sampl.frequ <- cumsum(sampl)/(1:trials)
	plot(sampl.frequ, ylim=c(.2, 1), type="l", xlab = "Number of trials", ylab = "average")
	abline(h = 0.5, col = "red")
	breaks = seq(0,1, by=0.1)
	sampl.cut = cut(sampl.frequ, breaks, right=FALSE)
	return(table(sampl.cut))
}
	
#Q2
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  
  samples <- switch(populationDistribution,
	"normal" = matrix(rnorm(sampleSize*numberOfSamples),numberOfSamples),
	"uniform" = matrix(runif(sampleSize*numberOfSamples),numberOfSamples))
	
  sample.means <- apply(samples,1,mean)
  hist(sample.means,col="gray",main="Sampling Distribution")
  result <- list("mean"=mean(sample.means), "se"=sd(sample.means)/sqrt(sampleSize))
  return(result)
}

#Q3a
# return: string('TV', 'Radio', 'Newspaper'), represents the covariate providing the best prediction
SLR <- function(path='../data/hw23R-Advertising.csv'){
    data <- read.csv(path)
	lm.fit.tv=lm(Sales~TV,data=data)
	lm.fit.radio=lm(Sales~Radio,data=data)
	lm.fit.np=lm(Sales~Newspaper,data=data)
	
	par(mfrow = c( 1, 3 ))
	plot(data$TV, data$Sales)
	abline(lm.fit.tv)
	plot(data$Radio, data$Sales)
	abline(lm.fit.radio)
	plot(data$Newspaper,data$Sales)
	abline(lm.fit.np)
      return('TV')
}

#Q3b 
# return: list of four variables, Intercept， TvCoeff，NewspaperCoeff，RadioCoeff
MLR <- function(path='../data/hw23R-Advertising.csv'){
  
  data <- read.csv(path)
  #plot(data[,2:5])
  lm.fit = lm(Sales ~ TV + Radio + Newspaper,data=data)
  lm.fit.coef <- lm.fit$coefficients
  #anova(lm.fit)
  result <- list("Intercept"=lm.fit.coef[1], "TVCoeff"=lm.fit.coef[2], "NewspaperCoeff"=lm.fit.coef[4], "RadioCoeff"=lm.fit.coef[3])
  return(result)
}
  
#Q4
# return: list of four variables, Intercept， X1Coeff，X2Coeff，X3Coeff
LogisticRegression <- function(path='../data/hw23R-q4data.txt'){ 
  data <- fread(path)
  mylogit <- glm(Y ~ X1 + X2 + X3, data = data, family = binomial(link = "logit"))
  #print(summary(mylogit)) 
  #plot(mylogit) 
  mylogit.coef <- mylogit$coefficients
  result <- list("Intercept"=mylogit.coef[1][1], "X1Coeff"=mylogit.coef[2][1], "X2Coeff"=mylogit.coef[3][1], "X3Coeff"=mylogit.coef[4][1])
  return(result)
}

#Q5
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-q4data.txt'){
  data <- fread(path)
  set.seed(3456)
  trainIndex <- createDataPartition(data$Y, p = .67,
                                  list = FALSE,
                                  times = 1)
  train.data <- data[ trainIndex,]
  test.data  <- data[-trainIndex,]
  model <- glm(Y ~ X1 + X2 + X3,data = train.data, family=binomial)
  pred <- predict(model, test.data, type = "response")
  cm <- table(round(pred),unlist(test.data[,1]))

   
  model <- glm(Y ~ X1 + (X3 + X2),data = train.data, family=binomial)
  pred <- predict(model, test.data, type = "response") 
  cm <- table(round(pred),unlist(test.data[,1]))
  acc = sum(diag(cm))/sum(cm)
  print(summary(model))

  model <- glm(Y ~ (X1 + X3) + X2,data = train.data, family=binomial)
  pred <- predict(model, test.data, type = "response") 
  cm <- table(round(pred),unlist(test.data[,1]))
  acc = sum(diag(cm))/sum(cm)
 
  model <- glm(Y ~ X1 + X3 + log(X2),data = train.data, family=binomial)
  pred  <- predict(model, test.data, type = "response") 
  cm <- table(round(pred),unlist(test.data[,1]))
  accuracy = sum(diag(cm))/sum(cm)

  return (accuracy)
}

#Q6
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
  data <- fread(path)
  data.half <- data[1:(nrow(data)*0.50),]
  data.lm = biglm(y~x,data=data.half)
  result <- list("Intercept"=coef(data.lm)[1], "xCoeff"=coef(data.lm)[2])

  set.seed(123) 
  data.one.percent <- data[sample(1:nrow(data),nrow(data)*.01,replace=FALSE),]
  data.one.percent.lm = lm(y~x,data=data.one.percent)

  set.seed(123) 
  data.two.percent <- data[sample(1:nrow(data),nrow(data)*.02,replace=FALSE),]
  data.two.percent.lm = lm(y~x,data=data.two.percent)

  set.seed(123) 
  data.three.percent <- data[sample(1:nrow(data),nrow(data)*.03,replace=FALSE),]
  data.three.percent.lm = lm(y~x,data=data.three.percent)

  set.seed(123) 
  data.four.percent <- data[sample(1:nrow(data),nrow(data)*.04,replace=FALSE),]
  data.four.percent.lm = lm(y~x,data=data.four.percent)

  set.seed(123) 
  data.five.percent <- data[sample(1:nrow(data),nrow(data)*.05,replace=FALSE),]
  data.five.percent.lm = lm(y~x,data=data.five.percent)
   
  plot(data.five.percent$'x', data.five.percent$'y', main="regresion")
  abline(data.two.percent.lm, col = "blue")
  abline(data.three.percent.lm, col = "pink")
  abline(data.four.percent.lm, col = "yellow")
  abline(data.five.percent.lm, col = "green")

  return(result)
}

