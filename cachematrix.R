#The functions below calculates the inverse of a matrix by caching previous calculations and results to be used 
#if the matrix values do not change

#This function creates a special object which is setting the value of the matrix, getting the value of the matrix, setting 
#the value of the inverse of the matrix and getting the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix(sample(1:100, 16), 4, 4)) {
i <- NULL
set <- function(y) {
  x <<- y
  i <<- NULL
}
get <- function() x
setinverse <- function(solve) i <<- solve
getinverse <- function() i
list (set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


#This function calculates the inverse of the matrix by checking first if it has been already calculated in which case
#it gets the result from the cache and skips the computation

cacheSolve <- function(x, ...) {
#Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}