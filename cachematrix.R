## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
