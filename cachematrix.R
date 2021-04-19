makeCacheMatrix <- function(x = matrix()){
  fred <- NULL
  set <- function(y){
    x <<- y
    fred <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) { <<- inverse}
  getInverse <- function() {fred} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  fred <- x$getInverse()
  if(!is.null(fred)){
    message("getting cached data")
    return(fred)
  }
  mat <- x$get()
  fred <- solve(mat, ...)
  x$setInverse(fred)
  fred
}
