## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function takes matrix as its input, sets the values of the matrix ,
## gets the value of the matrix and sets the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## This function checks if the matrix inverse is there or not,
## if not available then it gets the matrix data and sets the inverse
## if it has the inverse then a message is displayed and cached data is returned

cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()
  if(!is.null(inv)) 
  {
    message("getting the cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
