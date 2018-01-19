q9.part1 <- function(){
	sample <- rnorm(100)
	mean <- mean(sample)
	median <- median(sample)
	variance <- var(sample)
	sd <- sd(sample)
	quantile <- quantile(sample)
	cbind(mean, median,variance, sd, quantile)
}

q9.part2 <- function(){
	sample <- rnorm(100)
	hist(sample, breaks = "Sturges")
}

q9.part3 <- function(){
	sample <- rnorm(100)
	qqnorm(sample)
	qqline(sample)
}

q9.part4.a <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	summary(data)
}

q9.part4.b.height <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	hist(data[,1], main="Height (in inches)", xlab="Height")
}

q9.part4.b.weight <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	hist(data[,2], main="Weight (in pounds)", xlab="Weight")
}

q9.part4.c.height <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	boxplot(data[,1], main="Height (in inches)")	
}

q9.part4.c.weight <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	boxplot(data[,2], main="Weight (in pounds)")	
}


q9.part4.d <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	plot(data, main="Scatterplot", 
  	xlab="Height (in inches) ", ylab="Weight (in pounds)")
}

q9.part5.height <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	plot(data[,1],dnorm(data[,1], mean = 62, sd = 4), 
	xlab="Height (in inches)", main="Normal Distribution(mean = 62, sd = 4)",
	ylab="Density")
}

q9.part5.weight <- function(){
	data <- cbind(sample(45:80, size=100, replace = TRUE),
 			sample(10:250, size=100, replace = TRUE))
	plot(data[,2],dnorm(data[,2], mean = 120, sd = 30), 
	xlab="Weight (in pounds)", main="Normal Distribution(mean = 120, sd = 30)",
	ylab="Density")
}

q9.part6 <- function(){
	install.packages("mvtnorm", dependencies = TRUE)
	library(mvtnorm)
	install.packages("rgl", dependencies = TRUE)
	library(rgl)
	data <- cbind(seq(45,80, length=100), seq(10,250, length=100))
	height.inches <- data[,1]
	weight.pounds <- data[,2]
	z = outer(height.inches ,weight.pounds, function(x,y) dnorm(x,62,4)*dnorm(y,120,30))
	persp3d(height.inches,weight.pounds,z,col = rainbow(100))
}

