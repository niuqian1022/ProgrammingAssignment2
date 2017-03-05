## The first function makeCacheMatrix create a special "matrix", which is a list of function to 
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of matrix 'x' created by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m <-solve(matrix, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

