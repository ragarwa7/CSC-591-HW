file.scores <- function(){
	data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
}

part.a.q6 <- function(){
    data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
	mean.courses <- matrix(colMeans(data.scores),1,2)
	rownames(mean.courses) <- "mean"
	median.courses <- cbind(median(data.scores[,1]),median(data.scores[,2]))
	rownames(median.courses) <- "median"
	sd.courses <- cbind(sd(data.scores[,1]),sd(data.scores[,2]))
	rownames(sd.courses) <- "sd"
	range.courses <- cbind(max(data.scores[,1])- min(data.scores[,1]),max(data.scores[,2])- min(data.scores[,2]))
	rownames(range.courses) <- "range"
	courses <- rbind(mean.courses,median.courses,sd.courses,range.courses)
	colnames(courses) <- c("course1","course2")
	courses 
}

part.b.q6 <- function(){
      data.scores <- read.csv("hw1q6_new.csv", header = FALSE) 
	course.one <- quantile(data.scores[,1], c(.25, .50, .75)) 
	course.two <- quantile(data.scores[,2], c(.25, .50, .75)) 
	rbind(course.one,course.two)
}	

part.c.course1.q6 <- function(){
	data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
	course.one <- c(data.scores[,1])
	myhist <- hist(course.one, breaks=10, prob=TRUE, col="grey")
	lines(density(course.one))
}

part.c.course2.q6 <- function(){
	data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
	course.two <- c(data.scores[,2])
	myhist <- hist(course.two, breaks=10, prob=TRUE, col="grey")
	lines(density(course.two, , adjust = 2))
}

part.d.course1.q6 <- function(){
	data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
	course.one <- c(data.scores[,1])
	qqnorm(course.one)
	qqline(course.one)	
}

part.d.course2.q6 <- function(){
	data.scores <- read.csv("hw1q6_new.csv", header = FALSE)
	course.two <- c(data.scores[,2])
	y <- qunif(ppoints(length(course.two)))
	qqnorm(course.two)
	qqline(course.two)
	shapiro.test(course.two)
}

