q10.part1 <- function(){
	sample <- rnorm(100)
	mean <- mean(sample)
	median <- median(sample)
	variance <- var(sample)
	sd <- sd(sample)
	quantile <- quantile(sample)
	cbind(mean, median,variance, sd, quantile)
}



q10.part2 <- function(){
	sample <- rnorm(100)
	hist(sample, breaks = "Sturges")
}

q10.part3 <- function(){
	sample <- rnorm(100)
	qqnorm(sample)
	qqline(sample)
}

q10.part4.a <- function(){
	height.inches <- sample(45:80, size=100, replace = TRUE)
	weight.inches <- sample(10:250, size=100, replace = TRUE)
	data <- (cbind(height.inches,weight.inches))
	summary(data)
}

q10.part4.b <- function(){
	height.inches <- sample(45:80, size=100, replace = TRUE)
	weight.inches <- sample(10:250, size=100, replace = TRUE)
	data <- (cbind(height.inches,weight.inches))
	barplot(data[,2], main="Weight (in pounds)")
	barplot(data[,1], main="Height (in inches)")
}


q10.part4.c <- function(){
	height.inches <- sample(45:80, size=100, replace = TRUE)
	weight.inches <- sample(10:250, size=100, replace = TRUE)
	data <- (cbind(height.inches,weight.inches))
	boxplot(data[,2], main="Weight (in pounds)")
	#boxplot(data[,1], main="Height (in inches)")	
}


q10.part4.d <- function(){
	height.inches <- sample(45:80, size=100, replace = TRUE)
	weight.inches <- sample(10:250, size=100, replace = TRUE)
	data <- (cbind(height.inches,weight.inches))
	plot(data, main="Scatterplot", 
  	xlab="Height (in inches) ", ylab="Weight (in pounds)")
}


q10.part5.height <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	plot(data[,1],dnorm(data[,1], mean = 62, sd = 4), 
	xlab="Height (in inches)", main="Normal Distribution(mean = 62, sd = 4)",
	ylab="Density")
}

q10.part5.weight <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	plot(data[,2],dnorm(data[,2], mean = 120, sd = 30), 
	xlab="Weight (in pounds)", main="Normal Distribution(mean = 120, sd = 30)",
	ylab="Density")
}


q10.part6 <- function(){
	library(mvtnorm)
	install.packages("rgl", dependencies = TRUE)
	library(rgl)
	data <- cbind(seq(45,80, length=100), seq(10,250, length=100))
	height.inches <- data[,1]
	weight.pounds <- data[,2]
	z = outer(height.inches ,weight.pounds, function(x,y) dnorm(x,62,4)*dnorm(y,120,30))
	persp3d(height.inches,weight.pounds,z,col = rainbow(100))
}

