## Put comments here that give an overall description of what your
## functions do

## This function below creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##Creating function for special matrix
  i <- NULL ##Value of inverse i is set as null
  set <- function(y) { ##set the value of the vector
    x <<- y ## superassignment of y to x using to superassignment operator
    i <<- NULL ##superassigning NULL to inverse
  }
  get <- function() x ## get the value of the vector
  setinverse <- function(solve) i <<- solve ##set the value of the inverse
  getinverse <- function() i ##get the value of the inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function below computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) { ##calculates the inverse of the special matrix create with the above function 
  i <- x$getinverse() 
  if(!is.null(i)) { ## checks to see if the inverse has already been calculated
    message("getting cached data") ## displays this message if the inverse has been calculated and stored
    return(i) 
  }
  data <- x$get() ## If not calculated store value in data
  i <- solve(data, ...) ## Solve function computes inverse for square invertible matrix
  x$setinverse(i) 
  i
}
