iris.file <- function(){
   iris.data <- read.csv("iris.csv")
}


part.a <- function(){
   iris.data <- read.csv("iris.csv")
   plot(iris.data[,3], iris.data[,4], main="H1Q5-a plot", 
  	xlab="petal length", ylab="petal width")
}

part.b <- function(){
    iris.data <- read.csv("iris.csv")
    part.a()
    points(mean(iris.data[,3]), mean(iris.data[,4]),pch ="X")
    c(mean(iris.data[,3]), mean(iris.data[,4]))
}

part.c.minkowski <- function(r){
      iris.data <- read.csv("iris.csv")
      petals.x.distance <- (abs(iris.data[,3, drop = FALSE] - mean(iris.data[,3])))^r
	petals.y.distance <- (abs(iris.data[,4, drop = FALSE] - mean(iris.data[,4])))^r 
      minkowski.distance <- (petals.x.distance + petals.y.distance) ^ (1/r)
      minkowski.distance
}

part.c.euclidean <- function(){
     euclidean.distance <- part.c.minkowski(2)
     distance.p <- matrix(euclidean.distance)
     colnames(distance.p, do.NULL = FALSE)
     colnames(distance.p)[1] <- "distance from point P"
     distance.p
}

part.c.mahalanobis <- function(){
     iris.file
     petals.points <- iris.data[,c(3,4), drop = FALSE]
     distance.p <- matrix(mahalanobis(petals.points, colMeans(petals.points), cov(petals.points)))
     colnames(distance.p, do.NULL = FALSE)
     colnames(distance.p)[1] <- "Mahalanobis distance from point P"
     sqrt(distance.p)
}

part.c.city.block <- function(){
      part.c.minkowski(1)
}

part.c.chebyshev <- function(){
      iris.file
      number.row <- nrow(iris.data)
      petals.x.distances <- abs(matrix(iris.data[,3],number.row,1)- mean(iris.data[,3]))
	petals.y.distances <- abs(matrix(iris.data[,4],number.row,1) - mean(iris.data[,4]))
      chebyshev.distance <- pmax(petals.x.distances,petals.y.distances)
	chebyshev.distance 
}

part.c.cosine <- function(){
      iris.file
      petal.coordinates <- as.matrix(iris.data[,c(3,4), drop = FALSE])
      cosine.distance <- matrix(NA, nrow=nrow(petal.coordinates), ncol=1)
	petal.means <- matrix(colMeans(petal.coordinates),nrow=1,ncol=2)
	for (petal in 1:nrow(petal.coordinates)){
		 cosine.distance[petal,] <- dist.cosine(rbind(petal.coordinates[petal ,],petal.means)) 
	}
	cosine.distance
}

part.d.plot <- function(distances){
	iris.file
   	plot(iris.data[,3], iris.data[,4], main="H1Q5-d plot",xlab="petal length", ylab="petal width", type="n")
      closest.points <- matrix(nrow=10,ncol=2)
	distances.sort <- sort(c(distances),index.return = TRUE)
      for(i in 1:10){
	  closest.points[i,] <- as.matrix(iris.data[distances.sort[["ix"]][i],c(3,4),drop = FALSE])
	  text(closest.points[i,1],closest.points[i,2],label = i,cex= 0.7,pos = 4)
	  points(mean(iris.data[,3]), mean(iris.data[,4]),pch ="X")
	  points(closest.points[i,1],closest.points[i,2],col = i)	  
	}  
	closest.points 
}

part.d.euclidean.plot <- function(){
	distances <- part.c.euclidean()
	part.d.plot(distances)
}

part.d.mahalanobis.plot <- function(){
	distances <- part.c.mahalanobis()
	part.d.plot(distances)
}

part.d.city.block.plot <- function(){
	distances <- part.c.city.block()
	part.d.plot(distances)
}

part.d.minkowski.plot <- function(){
	distances <- part.c.minkowski(3)
	part.d.plot(distances)
}

part.d.chebyshev.plot <- function(){
	distances <- part.c.chebyshev()
	part.d.plot(distances)
}

part.d.cosine.plot <- function(){
	distances <- part.c.cosine()
	part.d.plot(distances)
}


