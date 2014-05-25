### makeCacheMatrix function : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

### cacheSolve function: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should 
retrieve the inverse from the cache.

cacheSolve<-function(x = matrix(),...)
{
  m <- x$getinv(x)
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get(x)
  m <- solve(data, ...)   ### Computing the inverse of a square matrix can be done with the solve function in R
  x$setinv(m)
  m
}

