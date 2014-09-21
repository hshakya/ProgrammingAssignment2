## The pair of functions written here cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse
##The first function makeCacheMatrix creates a special "matrix" which is a list to
##a) set the value of the matrix
##b) get the value of the matrix
##c) set the value of the inverse of the matrix
##d) get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
      x <- y
      inv <<- NULL
  }
  get <- function() x #get returns the value of x (argument of makeCacheMatrix)
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  #returns a labeled vector of functions set, get, setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function calculate the matrix of the special matrix created with the above function
## Works under the assumption that matrix is invertible
##it first checks whether inverse has been computed
## if yes it gets the inverse from the cache and skips the computation
## otherwise it calculates the inverse of the data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  #if not null, a valued was cached, so return inv
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  #since its null, set data to x from makeCacheMatrix
  data <- x$get() 
  inv <- solve(data, ...) #Calculate the inverse of the matrix
  x$setinverse(inv)
  inv
}
