## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  matrxInv <- NULL
  set <- function(y) 
    {
      x <<- y
      matrxInv <<- NULL
    }

  ## Get the value of the matrix.
  
  get <- function() x
  
  ## Set the inverse of the matrix.
  
  setInv <- function(i) matrxInv <<- i
  getInv <- function() matrxInv
  
  ## Get the inverse of the matrix.
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
  
  ## Get the inverse of the matrix.
  
  matrxInv <- x$getInv()
  
  ## Check if there is the matrix, if yes: print the message.
  
  if(!is.null(matrxInv)) 
    {
      print("getting cached data")
      return(matrxInv)
    }
  
  ## if not: get the inverse of the matrix.
  
  data <- x$get()
  matrxInv <- solve(data, ...)
  
  ## Set the inverse of the matrix.
  
  x$setInv(matrxInv)
  matrxInv
}
