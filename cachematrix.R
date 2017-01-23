## The two functions below are used to create a special object that can
## store a matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## This function first checks if the inverse of the Matrix is already calculated or not. If it is already calculated 
## then it displays the chached result OR else it calculates the inverse of the special "matrix" created by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
