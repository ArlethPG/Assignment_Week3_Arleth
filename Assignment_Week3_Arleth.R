
###Assignment_Week3_Arleth###


### First Part=>makeCacheMatrix ###

makeCacheMatrix <- function(M = matrix()) {
  invM <- NULL
  set <- function(x) {
    M <<- x
    invM <<- NULL
  }
  get <- function() M
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invM
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


### Example: Matrix (M2) ####

M1<-matrix(c(6,15,6,8),nrow=2) 
M2<-makeCacheMatrix(M1)
M2



### Second Part => cacheSolve ###

cacheSolve <- function(M, ...) {
  inv <- M$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invM)
  }
  mat <- M$get()
  invM <- solve(mat, ...)
  M$setInverse(invM)
  invM
}

### SOLUTION: EXAMPLE MATRIX (M2) ###


cacheSolve(M2)



