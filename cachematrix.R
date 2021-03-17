setwd("C:/Users/Siddharth si1215/Desktop/EN19CS301370 SIDDHARTH GOSAWI/DATA SCIENCE")
##
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  # setting the input argument to the x object in the parent environment
  # assigning the null to the M object in the parent environment - This line of code clears any value of m that had been cache
  #prior to the execution of cache mean
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  #if it is false, then the cache mean gets the vector from input object calculates a mean and uses the setmean function
  #on the input object to set the mean in the input object and then returns the value of the
  #mean to the parent environment by printing the mean of object
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

#to test my code try running -

# Matr <- makeCacheMatrix(matrix(c(1:16), c(4,4)))
# Matr$get()
# Matr$getmean()
# Matr$set(matrix(c(1:4),c(2,2)))
# cacheSolve(Matr)
# Matr$getmean()
