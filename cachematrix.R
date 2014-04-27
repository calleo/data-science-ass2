## -------------------------------------------------
## The functions makeCacheMatrix and cacheSolve will
## respectively setup a matrix object able to cache
## its inverse and later fetch the inverse using
## the cache or setting the cache.
##
## Example:
##
## m <- matrix(10:19,3,3)
## mCache <- makeCacheMatrix(m)
## mInv <- cacheSolve(mCache)
## # Called a 2nd time, cache is used!
## mInv <- cacheSolve(mCache)
## -------------------------------------------------

## Create a matrix object, with functions to
## retrieve, set and reset its cache

makeCacheMatrix <- function(x = matrix()) {
  ## Holds the inverse of X (the cache)
  inv <- NULL
  ## Re-initialize this object with new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Get the matrix of this object
  get <- function() x
  ## Set the inverse cache of X
  setinv <- function(i) inv <<- i
  ## Get the inverse using the cache
  getinv <- function() inv
  ## Create matrix cache object: A list
  ## of functions declared in makeCacheMatrix
  list(set = get, get = get,
    setinv = setinv,
    getinv = getinv)
}


## Get the matrix inverse of a matrix object
## created by makeCacheMatrix. The function will
## use the cache if present. Otherwise solve()
## will be called to calculate the inverse of X
## and the cache will be updated

cacheSolve <- function(x, ...) {
  ## Get cache
  inv <- x$getinv()
  if(!is.null(inv)){
    ## Cache is present, return inverse
    return(inv) 
  }
  ## No cache, calculate the inverse of X
  data <- x$get()
  inv <- solve(data, ...)
  ## Set cache and return inverse
  x$setinv(inv)
  inv
}
