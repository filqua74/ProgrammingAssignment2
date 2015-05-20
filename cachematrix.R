## Put comments here that give an overall description of what your
## functions do

## Creates a matrix which could cache its inverse
## Arg1: orginal matrix
## Ret : cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  	set <- function(y) {
    		x <<- y
    		i <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) i <<- inverse
  	getinverse <- function() i
  	list(set = set, get = get,
       	     setinverse = setinverse,
       	     getinverse = getinverse)
}


## Calculate the inverse of a matrix, caching the result if not already done
## Arg1: cacheable matrix
## Ret : inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
  	if(!is.null(m)) {
    		message("getting cached data")
    		return(m)
  	}
  	data <- x$get()
  	m <- solve(data, ...)
 	x$setinverse(m)
  	m
}
