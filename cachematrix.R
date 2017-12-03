## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly. Specifically, the
## makeCacheMatrix and cacheSolve functions can be used to cache the inverse of any matrix.

## Underlying assumption: the matrix supplied is always invertible. 

## The makeCacheMatrix creates a special "vector" or a list containing a function. 
## Specifically, it creates a special "matrix" object that can cache its inverse.
## This function will do the following:
## 1. Set the value of the vector;
## 2. Get the value of the vector;
## 3. Set the value of the inverse of the matrix; and
## 4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by the function
## above. Assuming the inverse has already been calculated and the matrix has not changed,
## then this function should be able to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # If inverse has already been calculated
        if (!is.null(inv)) {
          #gets  
          message("getting cached data")
            return(inv)
        }
        #
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## DEMONSTRATION: 2 x 2 matrix
## test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## test_matrix$get()