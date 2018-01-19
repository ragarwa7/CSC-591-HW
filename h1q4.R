part.a.q2 <- function(){
  heart.rates <- 84:112
  normalize.heart.rates <- lapply(heart.rates,function(heart.rates) (heart.rates - 98.6)/0.95)  
  #write.csv(as.matrix(normalize.heart.rates), file = "MyData.csv")
  myfile <- read.csv("MyData.csv")
  range(myfile[,2])
}

part.a.q4 <- function(){
   matrix.a <- matrix(rep(0,9),3,3)
   diag(matrix.a) <- 1
   matrix.a
}

part.b.q4 <-function(){
  second.column.a <- part.a.q4()
  second.column.a[,2] <- 3
  second.column.a
}

part.c.q4 <- function(){
  matrix.sum <- 0
  matrix.sample <- part.b.q4()
  for (i in seq_len(nrow(matrix.sample))){
	for (j in seq_len(ncol(matrix.sample))){
		matrix.sum = matrix.sum + matrix.sample[i,j]
      }
  }
  matrix.sum
}

part.d.q4 <- function(){
  matrix.sample <- part.b.q4()
  third.row <- matrix.sample[3,]
  second.column <- matrix.sample[,2]
  sum(cbind(third.row,second.column,diag(matrix.sample)))
}


part.e.q4 <- function(){
  matrix( rnorm(5*5,mean=7,sd=sqrt(1)), 5, 5)   
}

part.f.q4 <- function(){
   matrix.b <- part.e.q4()
   matrix.c <- matrix(nrow=2,ncol=ncol(matrix.b))
   matrix.c[1,] <-  matrix.b[1,] -  matrix.b[2,]
   matrix.c[2,] <-  matrix.b[3,] +  matrix.b[4,]
   matrix.c
}

part.g.q4 <- function(){
   matrix.c <- part.f.q4()
   matrix.d <- matrix.c * matrix(rep(2:(ncol(matrix.c)+1),each=nrow(matrix.c)),nrow=nrow(matrix.c)) 
   matrix.d 
}

part.h.q4 <- function(){
  matrix.x <- t(matrix(c(1,3,5,8),1,4))
  matrix.y <- t(matrix(c(5,3,2,1),1,4))
  cov(matrix.x)
  cov(matrix.y)
}

part.i.q4 <- function(){
  matrix.x <- t(matrix(c(1,3,5,8),1,4))
  sqaure.sd.x <- sd(matrix.x)^2
  square.mean.x <- mean(matrix.x)^2
  sqaure.matrix.x <- matrix.x * matrix.x
  if(mean(sqaure.matrix.x)==  square.mean.x + sqaure.sd.x){
    TRUE
  }else{
    FALSE
}
mean(sqaure.matrix.x)
sd(matrix.x)}

