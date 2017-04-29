## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix function set and get the value of matrix and set and get the value of inverse matrix

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


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

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

## Sample Run
## x = rbind(c(1, -1/4), c(-1/4, 1))
## m = makeCacheMatrix(x)
## m$get()
## cacheSolve(m)
## cacheSolve(m)



