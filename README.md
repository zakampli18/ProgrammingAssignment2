### makeCacheMatrix function

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

### cacheSolve function

cacheSolve<-function(x = matrix(),...)
{
  m <- x$getinv(x)
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get(x)
  m <- solve(data, ...)
  x$setinv(m)
  m
}

