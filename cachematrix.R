## Put comments here that give an overall description of what your
## functions do

## There are 2 functions in this program. makeCacheMatrix creates a special Matrix and set the value of the Matrix.
## It also creates a list for setting the Matrix / Inverse of the Matrix and Getting the Matrix and Inverse of the Matrix.

## Write a short comment describing this function
## This function creates a Special "Matrix". It is a list containing the following:
## 1) set the value of the Matrix
## 2) Get the value of the Matrix
## 3) Set the Value of Inverse of the Matrix
## 4) Get the Value of Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get function
  get <- function() x
  
  #setinv function
  setinv <- function(invParam) inv <<- invParam
  
  #getinv function
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# This function will check if the Matrix is Inversible. If it is inversible, then it will try to get the Inverse
# from Cache if already calculated for the same Matrix. If not, will calcualte the Inverse and set it in the Cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  # Check the Matrix to see if it is a Square Matrix and if there is a Determinant - only then 
  # the Matrix will have an Inverse
  tmp <- class(try(solve(x$get()),silent=T))=="matrix"
  if (tmp == 'FALSE'){
    stop("Matrix does not have an Inverse")
  }
  
  # Check if the inverse is already calcualted for this Matrix and Cached. If so, get the Cached Inverse       
  i <- x$getinv()
  if(!is.null(i)) {
    message("Inverse is alrady calculated and Cached. Getting cached data...")
    return(i)
  }
  # If not already done, calcuate the Inverse      
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i #return i
  
}
