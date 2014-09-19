## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(mCM) is actually four functions in one. Encapsulated within
## mCM is set, which adds the matrix to the list, get which retrieves the matrix,
## setinverse and getinverse, which sets and gets the inverses respectively.
## At the highest level this creates an object with which to store matrices.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves the matrix in makeCacheMatrix. It also stores the
## results in the list that the matrix object from 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
	  message("getting cached data:")
	  return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
