## these two functions are used to cache the inverse of a matrix

## it creates a list of functions to set the value ,get the value
##setinverse and getinverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     
     
}


## it returns the inverse of matrix.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

## > x=rbind(c(2,5),c(5,2))
##> y=makeCacheMatrix(x)
##> y$get()
#[,1] [,2]
#[1,]    2    5
#[2,]    5    2
#> cacheSolve(y)
#[,1]       [,2]
#[1,] -0.0952381  0.2380952
#[2,]  0.2380952 -0.0952381
#again
#> cacheSolve(y)
#getting cached data.
#[,1]       [,2]
#[1,] -0.0952381  0.2380952
#[2,]  0.2380952 -0.0952381