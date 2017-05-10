## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Createa  functoion
  ##Decalare variable for invrers  
  inv = NULL
  ##use `<<-` to assign a value to an object in an environment different from the current environment.
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  ## call x to see the matrix
  get = function() x
  ## set invers to a variable
  setinv = function(inverse) inv <<- inverse 
  ## get invers from a variable
  getinv = function() inv
  ##return a list
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## @x: is the output in of the makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  ##geting and invers
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  # get it from the cache and skips the computation. 
  return(inv)
}
