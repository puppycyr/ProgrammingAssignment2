# The functions below solve for the inverse of a matrix. 

# If the contents of the matrix are not changing, these functions cache the 
# inverse matrix so that when we need it again, it can be looked up in the 
# cache rather than recomputed. 

# If the contents of the matrix are changing, these functions calculate the
# inverse of the new matrix.



# The 1st function creates a special "matrix" object that can cache its inverse. 
# This function is a list containing a function to:
# 1) set the value of the matrix; 2) get the value of the matrix;
# 3) set the inverse matrix; and 4) get the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL  # initialization
  set <- function(y) { 
    x <<- y  # assign new matrix as input
    InverseMatrix <<- NULL  # initialize memeory
  }
  get <- function() x  ## get the new matrix
  setInverse <- function(im) InverseMatrix <<- im  # put into memory
  getInverse <- function() InverseMatrix  # read memory
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  # put together an inventory of functions
}



# The 2nd function calculates the inverse of the special "matrix" created with 
# the above function. 
# It first checks to see if the mean has already been calculated.
# If so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the inverse matrix
# in the cache via the setInverse function.

cacheSolve <- function(x) {
  # Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getInverse()  # read the result in memory
  if(!is.null(InverseMatrix)) {  # when the result in memeory is not null
    message("getting cached data")  # indicate there is no change
    return(InverseMatrix)  # return the result in memory
  }
  # when the result in memory is null, calculate the new inverse matrix
  newmatrix <- x$get()  # read in the new matrix
  InverseMatrix <- solve(newmatrix) # solve for the inverse matrix
  x$setInverse(InverseMatrix)  # put new result into memory
  InverseMatrix  # return this new result
}
