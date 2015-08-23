## Elaborated by : Julian Leonardo Garcia Pablos

## This function let us to create a object that stores a matrix and its inverse. 
## It is useful to know that this function don't calculate the inverse matrix or the matrix. 
## Also it let us get the matrix insted calculate it repeatly.
## if x its a boject created with this function, then we can use this commands : 
## x$set : it let us save the matrix
## x$get : it let us get the matrix to the console 
## x$setinv : it let us save the inverse matrix 
## x$getinv : it let us get the inverse of the matrix to the console 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## First, this function find the inverse into the objet that we create to store de matrix and it’s inverse. 
## If it find it then it print its value, if it can’t find it then it calculates the inverse. 
## It also save the inverse into the original object.
## This function don't create a object. it verify if the inverse was set in the object that was created with the previous function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv        
}
