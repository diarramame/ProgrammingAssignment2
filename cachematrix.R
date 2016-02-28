## Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean
## (1)get is a function that returns the vector x stored in the main function.
#(2)set is a function that changes the vector stored in the main function.
#(3)setmean and getmean are functions very similar to set and get.
#(4)They don’t calculate the mean, they simply store the value of the input in a variable m.
#(5)into the main function makeVector (setmean) and return it (getmean).


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mat <<- solve
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
