## Put comments here that give an overall description of what your
## functions do

## The 'makeCacheMatrix' function creates (similarly to the example exercise with vector)
## a list that contains original matrix and its inversion.
## See comments in the function body.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## assigning a NULL value object 'inv' to have a slot in memory ready for when 'inv' is required
  set <- function(y) { ## creating a function 'set' that assigns the 'x' the value 'y' and
    x <<- y        ## 
    inv <<- NULL   ## resets the 'inv' to be NULL in global environment
  }
  get <- function() x ## creates function 'get' that returns x (which is the input square, inversible matrix)
  setinverse <- function(inverse) inv <<- inverse ## creates function 'setinverse' that assigns 'inv' to be 'inverse'
  getinverse <- function() inv ## creates function 'getinverse' that calculates inverse of 'x' ('inv' is defined later in 'cacheSolve')
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## this list is the output (aka. what is returned) of makeCacheMatrix,
                                ## it sets the value of the matrix, then gets the value it was set,
                                ## then calculates the inverse of the set matrix,
                                ## and finally gets the value of the inverse matrix
  
}


## The 'cacheSolve' function calculates the inverse of matrix 'x'.
## It "knows" the value of 'x' and funtion inv because they were
## assigned using the super assignment operator <<- in 'makeCacheMatrix'
## this means that 'x' and inv are available in global environment wherere 'cacheSolve' ends up looking for operators


cacheSolve <- function(x, ...) { ## sets a function 'cacheSolve' to use matrix 'x' and do the following:
  inv <- x$getinverse() ## assings 'inv' the value of matrix x with 'getinverse' function
  if(!is.null(inv)) { ## if 'inv' already has a calculated value, then returns message "getting cached data"
    message("getting cached data") ## "prints out 'getting cached data'"
    return(inv) ## returns the value of 'inv'
  }
  data <- x$get() ## assigns data to be 'x' with get function (basically gets value of matrix 'x')
  inv <- solve(data, ...) ## assigns 'inv' to be a function that takes the inverse (solve function) of data
  x$setinverse(inv) ## operates function 'inv' to the matrix 'x'
  inv ## outputs the inverse of matrix 'x'
}
