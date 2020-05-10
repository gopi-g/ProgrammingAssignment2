## The function makeCacheMatrix creates list of functions can following
# 1.	set the value of the given matrix
# 2.	get the value of the given matrix
# 3.	set the value of the inverse of the given matrix
# 4.	get the value of the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  setmatrix <- function(m) {
    x <<- m
    inv1 <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(inv) inv1 <<- inv
  getinv <- function() inv1
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


## The function CacheSolve takes the makeCacheMatrix function as a argument
## and check whether inverse matrix is cached,  
## if it is not it calculates inverse and cache it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix ")
    return(inv)
  }
  data <- x$getmatrix()
  if (det(data) == 0){
    message("Given matrix is singular, Hence it is not invertable")
  }
  else{  
    inv <- solve(data, ...)
    x$setinv(inv)
    inv}
}