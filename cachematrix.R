## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# set up environment where things will be
  # invmat inverse of matrix
  invmat <- NULL
  # assigns matrix and invmat to the "parent" environment (when makeCacheMatrix was called)
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  #make a list with the "cached" data, each element is a function defined in an environment
  #which acts like the "cache label".
  #first the matrix
  get <- function() x
  #then set the inverse
  setinvmat <- function(im) invmat <<- im
  #then to get the inverse
  getinvmat <- function() invmat
  #put all in a list
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## Write a short comment describing this function
# gets the list containing the matrix and the cached data (if already calculated).
# if no cached data then it solves it
cacheSolve <- function(x, ...) {
  #check if invmat is not null.
  invmat <- x$getinvmat()
  if(!is.null(invmat)) {
    #is not, use the cached vakue
    message("getting cached data")
    return(invmat)
  }
  #invmat is null, get the matrix (data) from the list
  data <- x$get()
  #calculate the inverse
  invmat <- solve(data, ...)
  #set it in the cached list and return it
  x$setinvmat(invmat)
  invmat
        ## Return a matrix that is the inverse of 'x'
}
