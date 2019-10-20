##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Write a short comment describing this function
##Step 1: Initialize objects
##Step 2: Define the functions for objects of type makeVector()
##Step 3: Create a new object by returning a list()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(A) {
    x <<- A
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## If the result of !is.null(m) is FALSE, 
##cachemean() gets the vector from the input object, calculates a mean(), \
##uses the setmean() function on the input object to set the mean in the input object
##and then returns the value of the mean to the parent environment by printing the mean object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retreiving data that is cashed")
    return(i)
  }
  datasetvalue <- x$get()
  i <- solve(datasetvalue, ...)
  x$setinverse(i)
  i
}

