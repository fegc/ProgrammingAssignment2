## Implements two functions makeCacheMatrix and cacheSolve that allows to cache the inverse of a matrix


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  getInv <- function() invMatrix
  setInv <- function(inv) {invMatrix <<- inv}
  set <- function(newMatrix) x <<- newMatrix
  get <- function() x
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...)m <-  {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(is.null(invMatrix)){
    invMatrix <- solve(x$get())
    x$setInv(invMatrix)
  } else{
    invMatrix <- x$getInv()
  }
  print(invMatrix)
  
}
