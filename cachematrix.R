## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y){
    x <<- y      #assign input matrix to x
    mat_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mat_inverse <<- solve
  getinverse <- function() mat_inverse
  list(set = set, get = get,        #name the elements
       setinverse = setinverse,
       getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix
## if inverse is already calculated, it retrieves it from the cache

cacheSolve <- function(x, ...) {
  mat_inverse <- x$getinverse() # calls the getinverse function
  if(!is.null(mat_inverse)) {     # check if already has inverse cached
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data, ...)
  x$setinverse(mat_inverse)
  mat_inverse
        ## Return a matrix that is the inverse of 'x'
  ##solve(X) returns the inverse of a matrix
}
