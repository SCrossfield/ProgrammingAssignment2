## R Programming Assignment Wk 3: Caching the Inverse of a Matrix

## Matrix inversion is a useful but costly computation. 
## These functions enable the inverse of a matrix to be calculated and cached. 
## It can be called (retrieved) and also recalculated if the matrix is changed.

## makeCacheMatrix: this function creates a vector that is a list containing four functions:
## 1. Set the values of a matrix (set)
## 2. Get the values of a matrix (get)
## 3. Set the inverse of the matrix (setInverseMatrix)
## 4. Get the inverse of the matrix (getInverseMatrix)
## It takes a matrix as its input.
 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #m starts as null
  set<- function(y) {
    x <<- y #if set(myMatrix) is called, set x as myMatrix
    m <<- NULL #if set() is called, m is reset to null so that next get() call triggers an updated 
               #inverse matrix calculation rather than retrieving an old (incorrect) inverse matrix
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## cacheSolve: this function returns a matrix that is inverse of 'x'
## For a given matrix, the first time it also calculates the inverse and caches it

cacheSolve <- function(x, ...) {
  m <- x$getInverseM()
  if(!is.null(m)) { # if the inverse matrix is cached, retrieve this
    message("getting cached data")
    return(m)
  }
  mydata <- x$get() # else calculate the inverse matrix, cache and retrieve this
  m <- solve(mydata, ...)
  x$setInverseM(m)
  m
}
