## This function take and stores the inverse of an invertible matrix. 
## If the inverse already exist it doe not caluclate it again.

## this function cashe inverse of matrix in an object

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## this function computes the inverse of matrix if the inverse not in cache

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  # check if the inverse already exist
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # if not, calculate inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
