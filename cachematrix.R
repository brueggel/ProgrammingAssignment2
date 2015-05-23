## makeCacheMatrix is a function that contains 4 functions:
## set, get, setsolve, getsolve
## set- function that set square matrix; get- function that get stored matrix; 
## setsolve- function that set inverse for square matrix; 
## getsolve- function that get stored inverse for square matrix
## if matrix "x" is unchanged inverse for matrix(x) is stored in "m"
## if matrix "x" was changed to matrix(y), inverse in "m" become =NULL

makeCacheMatrix<- function(x= matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the square matrix returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then the
## "cacheSolve" retrieve the inverse from the cache.

cacheSolve<- function(x) { 
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
}
