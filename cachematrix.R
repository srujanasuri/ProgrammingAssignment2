## The main aim of this script is to read a matrix and to give the inverse of that perticular matrix
## This script consists of 2 functions "makeCacheMatrix" and "cacheSolve"

## "makeCacheMatrix()" reads the 'matrix' object that can cache its inverse.
## It consits of a list of 4 functions 'set', 'get', 'setinv', 'getinv'.

makeCacheMatrix <- function(mtx = matrix()) 
{
    inverse <- NULL
    set <- function(x)
    {
      mtx <<- x;
      inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) 
    {
      inverse <<- inv;
    }
    getinv <- function() return(inverse);
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## "cacheSolve()" computes the inverse of the 'matrix' returned by "makeCacheMatrix()" above.
## If the inverse has already been calculated (and the matrix has not changed), then
## "cacheSolve()" should retrieve the inverse from the cache.


cacheSolve <- function(mtx, ...)  
{
  inverse <- mtx$getinv()
  if(!is.null(inverse))
  {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  inverse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
