## makeCacheMatrix creates a special 'matrix' (cachematrix) object with four
## methods: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes in a 'cachematrix' object and returns the inverse from
## cache if it exists, otherwise it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## function testing below
z <- matrix(rnorm(16), 4,4)
x <- makeCacheMatrix()
x$set(z)
x$get()
cacheSolve(x)
cacheSolve(x)
x$getinverse()
