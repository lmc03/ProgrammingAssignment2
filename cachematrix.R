## The functions create a special matrix object that can cache the inverse of the matrix
## so that it would not recompute the inverse unnecessarily.

## makeCacheMatrix creates a special matrix that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  getinverse = NULL #getinverse will store the inverse of the matrix
  set = function(y) { #set the value of the matrix
    x <<- y
    getinverse <<- NULL
  }
  get = function() x #get the value of the matrix
  setinv = function(inv) getinverse <<- inv #set the value of getinverse
  getinv = function() getinverse #get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function computes for the inverse of the special "matrix"
## which was returned by the function above. If the inverse of the matrix was
##already calculated (and the matrix has not changed), the cacheSolve function would
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  getinverse = x$getinv()
  if(!is.null(getinverse)) { #enters the if statement if getinverse is null
    message("getting cached data")
    return(getinverse)
  }
  data = x$get()
  getinverse = solve(data, ...) #solve for the inverse of the matrix and store in getinverse
  x$setinv(getinverse)
  getinverse
}
