##makeCacheMatrix and cacheSolve work in tandem to find,
##store, and return the inverse of a matrix

## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##Returns the inverse of a special matrix, returns cached inverse if available,
##otherwise calculates the inverse and saves it to the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}



### Course Functions, used for understanding how cache worked
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   # set <- function(y) {
#   #   x <<- y
#   #   m <<- NULL
#   # }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list( get = get,
#         setmean = setmean,
#         getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }