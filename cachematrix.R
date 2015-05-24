## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.This is a pair of functions that
## cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(ma = matrix()) 
{
  inv <- NULL
  set <- function(x) 
    {
      ma <<- x;
      inv <<- NULL;
  }
  get <- function() return(ma);
  setinverse <- function(inverse) inv <<- inverse;
  getinverse <- function() return(inv);
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(ma, ...) 
{
  inv <- ma$getinverse()
  if(!is.null(inv)) 
    {
      message("getting cached data...")
      return(inv)
    }
  data <- ma$get()
  inv <- solve(data, ...)
  ma$setinverse(inv)
  return(inv)
}
