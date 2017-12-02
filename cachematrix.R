## The functions  below creates a list of matrix composed of the  stored matrix
##  and inverted matrix [using solve() function]
##----------------------------------------------------------------------------------

## The function makeCacheMatrix() creates a list that:
## 1. store [set] the value of the "invertable matrix"
## 2. compute [get] the value of the "invertable matrix"
## 3. store [set] the value of the inverse of the stored "invertable matrix"
## 4. store [set] the value of the inverse of the stored "invertable matrix"

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##----------------------------------------------------------------------------------
## The function cacheSolve() returns the inverted matrix from the 
## stored [cached matrix] "invertable matrix"
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}